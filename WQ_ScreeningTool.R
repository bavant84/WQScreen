library(dplyr)
library(reshape2)
library(tidyr)
#This tool was created by Brian Avant
#The purpose of this tool is to screen GKM related datasets containing metals concentrations in the water column against water quality standards for specific areas.

#Read in screening critiera and sample data
##NOTE: It is very important that you preserve numeric precision in your input files!
setwd("L:/Priv/AnimasRiver/ARP_River_Modeling/WASP_Modeling/WASP Appendices/WQScreen")
WQCritSS <-  read.table("WQCriteriaTot.txt",sep="\t",skip =0, header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)
ObsTot <- read.table("ObservedData_CurrentNDsasZero_HistNDsasLim.txt",sep="\t",skip =0, header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)
WQCritHardness <- read.table("WQCriteriawHardness.txt",sep="\t",skip =0, header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)
#Reformat WQ Screening Criteria
WQCritmelt <- melt(WQCritSS,id.vars=c("Designated_Use","ScreenType","Region","Sample_Type"))
WQCritSS_clean <- WQCritmelt[complete.cases(WQCritmelt),]
WQCritSS_clean$variable <- as.character(WQCritSS_clean$variable, stringsAsFactors = FALSE)
namevector <- c("maSlope","mbIntercept", "alphaBeta", "conversionFactor", "alpha", "beta")
WQCritSS_clean[,namevector] <- NA
WQCritAll <- rbind(WQCritSS_clean,WQCritHardness)
#Reformat Obs data to rerun overlapping regions
Tribes <- ObsTot[complete.cases(ObsTot[,3]),]
Tribes2 <- ObsTot[complete.cases(ObsTot[,4]),]
colnames(Tribes) [3] <- "Region"
colnames(Tribes2) [4] <- "Region"
colnames(ObsTot) [2] <- "Region"
ObsAllRegions <- rbind(ObsTot[,-c(3:4)],Tribes[,c(-2,-4)],Tribes2[,-c(2:3)])
#Cap hardness values based on specific criteria
obsCapped <- within(ObsAllRegions, Hardness[Hardness>400] <- 400) #Maximum hardness of 400 mg/L for most criteria in the region
#create output data.frames
rows <- nrow(WQCritSS_clean)
output_screen <- data.frame(Designated_Use = character(rows), ScreenType = character(rows), Region = character(rows), River = character(rows), 
                            Time_Period = character(rows), Sample_Type = character(rows), Metal = character(rows), 
                            Times_Exceeded = numeric(rows), Number_Screened = numeric(rows), stringsAsFactors=FALSE)
#This is the main function of the tool. For each sample the applicable screening criteria are identified and used to 
## determine the number of times a WQ criteria has been exceeded for a specific screen.
i=0
j=0
b=1
m=0
y=1
z=1
UniqueObs <- unique(obsCapped[c("Region","Sample_Type","River","Time_Period")]) 
if (file.exists("echoFile.txt")) file.remove("echoFile.txt") #remove old echo file before program starts
for (i in 1:nrow(UniqueObs)) { #loops through each sample by unique combinations of region and conc type(row)
    print(UniqueObs[i,])
    for (j in 10:ncol(obsCapped)){ #loops through each metal
        tempSamples <- filter(obsCapped, Region==UniqueObs[i,1], Sample_Type==UniqueObs[i,2], River==UniqueObs[i,3], Time_Period==UniqueObs[i,4]) #subset observed data by unique combination
        print(colnames(tempSamples[j]))
        if (UniqueObs[i,1]=="New Mexico" & UniqueObs[i,2]=="Total" & colnames(tempSamples[j])=="Aluminum") { #New Mexico hardness limit for total Al = 220 mg/L
            tempSamples <- within(tempSamples, Hardness[Hardness>220] <- 220)
        }
        hardness <- data.frame("Hardness" = tempSamples$Hardness, "Conc" = tempSamples[j], "ObsMetal" = colnames(tempSamples[j]), stringsAsFactors=FALSE) 
        screen <- filter(WQCritAll, Region==UniqueObs$Region[i], Sample_Type==UniqueObs$Sample_Type[i],variable==colnames(tempSamples[j])) #iteratively queries WQ criteria based on sample data (sample & metal)
        if (length(screen$value) > 0){
            for (b in 1:length(screen$ScreenType)) { #loop through matching screens 
                if (!is.na(screen$maSlope[b]==TRUE)) { #find screens that need to be calculated based on hardness
                     aquatic_screen <- data.frame(Designated_Use = character(nrow(tempSamples)), ScreenType = character(nrow(tempSamples)),Region = character(nrow(tempSamples)),
                                                  Sample_Type = character(nrow(tempSamples)),CritMetal = character(nrow(tempSamples)),
                                                  CalcValue = numeric(nrow(tempSamples)),SampleValue = numeric(nrow(tempSamples)),
                                                  ObsMetal = character(nrow(tempSamples)),stringsAsFactors=FALSE)
                     g=1
                    if (screen$alphaBeta[b] == 0) { #calculator function 1 
                        for (y in 1:nrow(hardness)) { #iterate through each sample 
                            screen$value[b] <- as.numeric((exp((screen$maSlope[b]*log(hardness$Hardness[y]))+screen$mbIntercept[b])*screen$conversionFactor[b])/1000) #calculate criteria
                            aquatic_screen[g,] <- c(screen$Designated_Use[b], screen$ScreenType[b], screen$Region[b],screen$Sample_Type[b],screen$variable[b],screen$value[b], hardness[y,2], hardness[y,3]) #collect criteria and sample value (for screen eval)
                            aquatic_screen[, c(6:7)] <- sapply(aquatic_screen[, c(6:7)], as.numeric)
                            g=g+1
                        }
                    } else if (screen$alphaBeta[b] == 1) { #calculator function 2 
                        for (z in 1:nrow(hardness)) { #iterate through each sample
                            screen$value[b] <- as.numeric((exp((screen$maSlope[b]*log(hardness$Hardness[z])+screen$mbIntercept[b]))*(screen$alpha[b]-(log(hardness$Hardness[z])*screen$beta[b])))/1000) #calculate criteria
                            aquatic_screen[g,] <- c(screen$Designated_Use[b], screen$ScreenType[b], screen$Region[b],screen$Sample_Type[b],screen$variable[b],as.numeric(screen$value[b]), as.numeric(hardness[z,2]), hardness[z,3]) #collect criteria and sample value (for screen eval)
                            aquatic_screen[, c(6:7)] <- sapply(aquatic_screen[, c(6:7)], as.numeric)
                            g=g+1
                        }
                    } else {
                        cat("Something went wrong with the hardness calculator.", "The error occured calculating the screening criteria for",screen$Sample_Type[b],
                            screen$variable[b], "using the",screen$ScreenType[b], "screen for",screen$Region[b])
                    }
                     aquatic_screen_cleaned <- filter(aquatic_screen, CalcValue >= 0, SampleValue >= 0) #remove empty rows in data.frame
                     n_screened <- nrow(aquatic_screen_cleaned) #count the number of samples that are screened
                     n_screened[is.null(n_screened)] <- -500
                     if (n_screened > 0) {
                         metal_vector_exceedances <- which(aquatic_screen_cleaned$SampleValue > aquatic_screen_cleaned$CalcValue) #filter criteria with exceedances
                         metal_exceedance_count <- length(metal_vector_exceedances) #count exceedances
                         m=m+1
                         output_screen[m,] <- c(screen$Designated_Use[b], screen$ScreenType[b], screen$Region[b], UniqueObs[i,3], UniqueObs[i,4], screen$Sample_Type[b],screen$variable[b],metal_exceedance_count, n_screened)
                    }
                    
                } else {
                    metal_df <- tempSamples[screen$variable[b]]
                    if (!all(is.na(tempSamples[screen$variable[b]]))) { #distinguishes between a non-detect sample and no sample
                        metal_vector_nonas <- metal_df[!is.na(metal_df)] #remove NAs
                        num_metal_samples <- length(metal_vector_nonas) #count the number of samples that are screened
                        num_metal_samples[is.null(num_metal_samples)] <- -500
                        if (num_metal_samples > 0) {
                            metal_vector_exceedances <- metal_vector_nonas[which(metal_vector_nonas>screen$value[b])] #filter criteria with exceedances
                            metal_exceedance_count <- length(metal_vector_exceedances) #count exceedances
                            m=m+1
                            output_screen[m,] <- c(screen$Designated_Use[b], screen$ScreenType[b], screen$Region[b], UniqueObs[i,3], UniqueObs[i,4], screen$Sample_Type[b],screen$variable[b],metal_exceedance_count, num_metal_samples) 
                            }
                    }
                }
            }
        } else {
            cat(UniqueObs$Sample_Type[i], colnames(tempSamples[j]), UniqueObs$Region[i], file="echoFile.txt", append=TRUE)
            }
        }
}
#write.csv(aquatic_screen, file="aquatic_screen.csv", row.names=FALSE)
output_screen <- filter(output_screen, ScreenType!="")
output_screen$Times_Exceeded <- as.numeric(output_screen$Times_Exceeded)
output_screen_Exceeded <- filter(output_screen, Times_Exceeded > 0)
write.csv(output_screen, file="WQScreen_Results_ALL.csv", row.names=FALSE)
write.csv(output_screen_Exceeded, file="WQScreen_Results_Exceeded.csv", row.names=FALSE)
write.csv(WQCritAll, file="WQScreen_Criteria.csv", row.names=FALSE)
##########################################################################################################################################################################