library(shiny)      
library(plyr)           
library(leaflet)    
library(reshape2)
library(tidyr)      
library(rpivotTable)    
library(dplyr)      
library(jsonlite)
library(rgdal)      
library(RJSONIO)        
library(tibble)     
library(stringr)
library(sp)         
library(maps)           
library(maptools)   
library(geojsonio)
library(ggplot2)    
library(shinydashboard) 
library(rjson)      
library(DT)
library(xlsx)       
library(readxl)
#install.packages("rjson")
## This tool was created by Brian Avant
## The purpose of this tool is to screen GKM related datasets containing metals concentrations in the water column against water quality standards for specific areas.

## Read in screening critiera and sample data
setwd("C:/Users/bavant/Dropbox/WQScreen") #work /Git/WQScreen
# setwd("C:/Users/Brian/Dropbox/WQScreen") #laptop wd
WQCritSS <- read_excel("WQ Criteria and Sample Templates.xlsx", sheet = "WQCriteriaTot")
WQCritHardness <- read_excel("WQ Criteria and Sample Templates.xlsx", sheet = "WQCriteriawHardness")

## Reformat WQ Screening Criteria
WQCritSS_clean <- WQCritSS %>%
    gather(variable, value, -c(Designated_Use,ScreenType,NAME,Spatial_Type,Sample_Type)) %>%
    filter(complete.cases(.))

namevector <- c("maSlope","mbIntercept", "alphaBeta", "conversionFactor", "alpha", "beta")
WQCritSS_clean[,namevector] <- NA
WQCritAll <- bind_rows(WQCritSS_clean,WQCritHardness)

## Create Output data.frames
rows <- nrow(WQCritSS_clean)

Samplemarkerlayer <- data.frame(Date_Time = character(rows*10),
                                Sample_No = character(rows*10),
                                Designated_Use = character(rows*10), 
                                Sp_Layer = character(rows*10),
                                ScreenType = character(rows*10), 
                                Lat = numeric(rows*10),
                                Lon = numeric(rows*10),
                                NAME = character(rows*10), 
                                Sample_Type = character(rows*10), 
                                CritMetal = character(rows*10), 
                                CalcValue = numeric(rows*10), 
                                SampleValue = numeric(rows*10),
                                ObsMetal = character(rows*10),
                                stringsAsFactors=FALSE)
#################### Load GEOJSONs and Merge Criteria Data #####################
statesJSON <- readOGR(dsn="selected_states.geojson", layer = "selected_states", verbose = FALSE) 
#statesJSON <- readOGR("selected_states.geojson", "OGRGeoJSON", verbose = FALSE) #selected_
states <- map(statesJSON, fill=TRUE, col="transparent", plot=FALSE)
StateIDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
states_sp <- map2SpatialPolygons(states, IDs=StateIDs,
                                 proj4string=CRS("+proj=longlat +datum=WGS84"))
tribesJSON <- readOGR(dsn="tribes.geojson", layer = "tribes", verbose = FALSE)
#tribesJSON <- readOGR("tribes.geojson", "OGRGeoJSON", verbose = FALSE)
tribesmap <- map(tribesJSON, fill=TRUE, col="transparent", plot=FALSE)
TribesIDs <- sapply(strsplit(tribesmap$names, ":"), function(x) x[1])
tribes_sp <- map2SpatialPolygons(tribesmap, IDs=TribesIDs,
                                 proj4string=CRS("+proj=longlat +datum=WGS84"))
regionsJSON <- readOGR(dsn="EPA_regions.geojson", layer = "EPA_regions", verbose = FALSE)
#regionsJSON <- readOGR("EPA_regions.geojson", "OGRGeoJSON", verbose = FALSE)
regions <- map(regionsJSON, fill=TRUE, col="transparent", plot=FALSE)
RegionsIDs <- sapply(strsplit(regions$names, ":"), function(x) x[1])
regions_sp <- map2SpatialPolygons(regions, IDs=RegionsIDs,
                                  proj4string=CRS("+proj=longlat +datum=WGS84"))
### latlong Conversion Function #######################################################
latlong2state <- function(pointsDF) {
    ## Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(pointsDF, 
                              proj4string=CRS("+proj=longlat +datum=WGS84"))
    ## Use 'over' to get _indices_ of the Polygons object containing each point 
    states_indices <- over(pointsSP, states_sp)
    ## Return the state names of the Polygons object containing each point
    stateNames <- sapply(states_sp@polygons, function(x) x@ID)
    stateNames[states_indices]
}
latlong2tribe <- function(pointsDF) {
    ## Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(pointsDF, 
                              proj4string=CRS("+proj=longlat +datum=WGS84"))
    ## Use 'over' to get _indices_ of the Polygons object containing each point 
    tribes_indices <- over(pointsSP, tribes_sp)
    ## Return the state names of the Polygons object containing each point
    tribeNames <- sapply(tribes_sp@polygons, function(x) x@ID)
    tribeNames[tribes_indices]
}
latlong2region <- function(pointsDF) {
    ## Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(pointsDF, 
                              proj4string=CRS("+proj=longlat +datum=WGS84"))
    ## Use 'over' to get _indices_ of the Polygons object containing each point 
    regions_indices <- over(pointsSP, regions_sp)
    ## Return the state names of the Polygons object containing each point
    regionNames <- sapply(regions_sp@polygons, function(x) x@ID)
    regionNames[regions_indices]
}
####################################################################################
m=0
n=0
g=0
#df <-  read.table("C:/Users/bavant/Dropbox/WQScreen/ObservedData_CurrentNDsasZero_HistNDsasLim_latlon_partial2.txt", header = TRUE, sep = "\t",stringsAsFactors=FALSE)
df <-  read.csv("C:/Users/bavant/Dropbox/WQScreen/GKM All Samples by Named Location.csv", header = TRUE, sep = ",",stringsAsFactors=FALSE)
#df <-  read.csv("C:/Users/Brian/Dropbox/WQScreen/New 2017 data for screening.csv", header = TRUE, sep = ",",stringsAsFactors=FALSE) #laptop
if (input$Spatialdist == "LatLon") { #lat lon version
    ## Sample Sites
    samplemarkers <- select(df, c(Lon,Lat,Samp_No))
    #write.csv(df,"samplemarkers.csv", row.names=FALSE)
    ## Collect relevant spatial boundaries from sample lat lon
    samplecoords <- select(df, c(Lon,Lat))
    Spatial_Boundstate <- str_to_title(latlong2state(samplecoords))
    Spatial_Boundregion <- str_to_title(latlong2region(samplecoords))
    Spatial_Boundtribe <- str_to_title(latlong2tribe(samplecoords))

    ## add States column to sample data and remove NAs
    ObsSpatial_BoundsStatena <- add_column(df, Spatial_Boundstate, .after = 1)
    ObsSpatial_BoundsState <- complete.cases(ObsSpatial_BoundsStatena[,2])
    ObsAllSpatial_BoundsState <- ObsSpatial_BoundsStatena[ObsSpatial_BoundsState, ]
    States_Layer <- add_column(ObsAllSpatial_BoundsState, Sp_Layer = "States", .after = 2)
    colnames(States_Layer)[2] <- "NAME"

    ## add EPA Region column to sample data and remove NAs
    ObsSpatial_BoundsRegionna <- add_column(df, Spatial_Boundregion, .after = 1)
    ObsSpatial_BoundsRegion <- complete.cases(ObsSpatial_BoundsRegionna[,2])
    ObsAllSpatial_BoundsRegion <- ObsSpatial_BoundsRegionna[ObsSpatial_BoundsRegion, ]
    Regions_Layer <- add_column(ObsAllSpatial_BoundsRegion, Sp_Layer = "Regions", .after = 2)
    colnames(Regions_Layer)[2] <- "NAME"

    ## add Tribe column to sample data and remove NAs
    ObsSpatial_BoundsTribena <- add_column(df, Spatial_Boundtribe, .after = 1)
    ObsSpatial_BoundsTribe <- complete.cases(ObsSpatial_BoundsTribena[,2])
    ObsAllSpatial_BoundsTribe <- ObsSpatial_BoundsTribena[ObsSpatial_BoundsTribe, ]
    Tribes_Layer <- add_column(ObsAllSpatial_BoundsTribe, Sp_Layer = "Tribes", .after = 2)
    colnames(Tribes_Layer)[2] <- "NAME"

    ## append all sample boundaries to one df
    ObsAllSpatial_Bounds <- rbind(States_Layer,Regions_Layer,Tribes_Layer)

} else { # instead of lat lon user provides columns declaring spatial boundaries
    df2 <- add_column(df, Sp_Layer = "State", Lat = NA, Lon = NA, .after = 2)
    Tribes_col <- df2[complete.cases(df2$Tribe),]
    if (nrow(Tribes_col) > 0) {Tribes_col$Sp_Layer <- "Tribes"}
    Tribes_col2 <- df2[complete.cases(df2$Secondary_Tribe),]
    if (nrow(Tribes_col2) > 0) {Tribes_col2$Sp_Layer <- "Tribes"}
    Regions_col <- df2[complete.cases(df2$Region),]
    if (nrow(Regions_col) > 0) {Regions_col$Sp_Layer <- "Regions"}
    names(Tribes_col)[names(Tribes_col)=="Tribe"] <- "NAME"
    names(Tribes_col2)[names(Tribes_col2)=="Secondary_Tribe"] <- "NAME"
    names(Regions_col)[names(Regions_col)=="Regions"] <- "NAME"
    names(df2)[names(df2)=="State"] <- "NAME"
    ObsAllSpatial_Bounds <- rbind(df2[, -which(names(df2) %in% c("Tribe","Secondary_Tribe","Region"))],
                                  Regions_col[, -which(names(Regions_col) %in% c("State","Tribe","Secondary_Tribe"))],
                                  Tribes_col[, -which(names(Tribes_col) %in% c("State","Secondary_Tribe","Region"))],
                                  Tribes_col2[, -which(names(Tribes_col2) %in% c("State","Tribe","Region"))])
    ObsAllSpatial_Bounds <- filter(ObsAllSpatial_Bounds, NAME != "")
 }
## Cap hardness values based on specific criteria
obsCapped <- within(ObsAllSpatial_Bounds, Hardness[Hardness>400] <- 400) #Maximum hardness of 400 mg/L for most criteria in the region
index <- 1 + which(colnames(obsCapped)=="Hardness" )
samples_long <- gather(obsCapped, "variable", "conc", index:ncol(obsCapped))

#if (input$Categories==TRUE)  {
    GroupCategories <- colnames(samples_long) [(which(colnames(samples_long)=="Lon")+1):(which(colnames(samples_long)=="Hardness")-1)]
    ScreenCategories <- c(GroupCategories, "variable")
    samples_long <- samples_long %>% mutate(Sample_Type = ifelse(NAME=="New Mexico" & Sample_Type=="Total" & variable=="Aluminum",
                                                 "Total Recoverable", 
                                                 Sample_Type))
    UniqueObs <- unique(samples_long[ScreenCategories])
    OutputCategories <- c("Designated_Use","ScreenType",GroupCategories,"Metal","Times_Exceeded","Number_Screened")

    output_screen <- data.frame(matrix(ncol = length(OutputCategories), nrow = rows),
                                stringsAsFactors=FALSE)
    names(output_screen) <- OutputCategories
    output_screen[,OutputCategories] <- lapply(output_screen[,OutputCategories],as.character)
    output_screen$Times_Exceeded <- as.numeric(output_screen$Times_Exceeded)
    output_screen$Number_Screened <- as.numeric(output_screen$Number_Screened)
    output_screen$Times_Exceeded[is.na(output_screen$Times_Exceeded)] <- 0
    output_screen$Number_Screened[is.na(output_screen$Number_Screened)] <- 0
    output_screen[is.na(output_screen)] <- ""
# } else {
    # for (i in 1:nrow(samples_long)) {
    #     samples_long$Sample_Type[i] = ifelse(samples_long$NAME[i]=="New Mexico" &
    #                                              samples_long$Sample_Type[i]=="Total" &
    #                                              samples_long$variable[i]=="Aluminum",
    #                                          "Total Recoverable",
    #                                          samples_long$Sample_Type[i])}
    # UniqueObs <- unique(samples_long[c("NAME","Sample_Type", "variable")])
    # output_screen <- data.frame(Designated_Use = character(rows),
    #                             ScreenType = character(rows),
    #                             NAME = character(rows),
    #                             Sample_Type = character(rows),
    #                             Metal = character(rows),
    #                             Times_Exceeded = numeric(rows),
    #                             Number_Screened = numeric(rows),
    #                             stringsAsFactors=FALSE)
#}

## This is the main function of the tool. For each sample the applicable screening criteria are identified and used to 
## determine the number of times a WQ criteria has been exceeded for a specific screen.
    for (i in 1:nrow(UniqueObs)) { #loops through each sample by unique combinations of region, conc type(row), and metal
        print(UniqueObs[i,])
        
        screen <- filter(WQCritAll, NAME==UniqueObs$NAME[i], #iteratively queries WQ criteria based on sample data (sample & metal)
                         variable==UniqueObs$variable[i],
                         Sample_Type==UniqueObs$Sample_Type[i]) 
        
        if (length(screen$value) > 0){
            #if (input$Categories==TRUE)  { # Converts designated columns into Categories
            filtercolumns <- which((names(samples_long) %in% names(UniqueObs[i,]))==TRUE)
            filt1 <- NULL
            filt2 <- NULL
            filtervar <- NULL
            for (l in 1:length(filtercolumns)){ # generates variable with string to pass to filter_
                filt1[l] <- names(samples_long[filtercolumns[l]])
                filt2[l] <-UniqueObs[i,l]
                filtervar[l] <-paste(filt1[l],"==","'",filt2[l],"'", sep="")
            }
            tempSamples <- samples_long %>% filter(UQ(rlang::sym(filt1[1]))==filt2[1]) %>%
                filter(UQ(rlang::sym(filt1[2]))==filt2[2]) %>%
                filter(UQ(rlang::sym(filt1[3]))==filt2[3]) %>%
                filter(UQ(rlang::sym(filt1[4]))==filt2[4]) %>%
                filter(UQ(rlang::sym(filt1[5]))==filt2[5])
            
            # } else {
            #     tempSamples <- filter(samples_long, NAME==UniqueObs$NAME[i], Sample_Type==UniqueObs$Sample_Type[i], variable == UniqueObs$variable[i]) #subset observed data by unique combination
            #}
            
            if (UniqueObs$NAME[i]=="New Mexico" & 
                UniqueObs$Sample_Type[i]=="Total" & 
                UniqueObs$variable[i]=="Aluminum") { #New Mexico hardness limit for total Al = 220 mg/L
                tempSamples <- tempSamples %>% within(Hardness[Hardness>220] <- 220) %>%
                    mutate(Sample_Type = "Total Recoverable",
                           conc = conc*0.31)
            }
            
            for (b in 1:length(screen$ScreenType)) { #loop through matching screens 
                if (!is.na(screen$maSlope[b]==TRUE)) { #find screens that need to be calculated based on hardness
                    aquatic_screen <- data.frame(Date_Time = character(nrow(tempSamples)),
                                                 Sample_No = character(nrow(tempSamples)),
                                                 Designated_Use = character(nrow(tempSamples)),
                                                 Sp_Layer = character(nrow(tempSamples)),
                                                 ScreenType = character(nrow(tempSamples)),
                                                 Lat = numeric(nrow(tempSamples)),
                                                 Lon = numeric(nrow(tempSamples)),
                                                 NAME = character(nrow(tempSamples)),
                                                 Sample_Type = character(nrow(tempSamples)),
                                                 CritMetal = character(nrow(tempSamples)),
                                                 CalcValue = numeric(nrow(tempSamples)),
                                                 SampleValue = numeric(nrow(tempSamples)),
                                                 ObsMetal = character(nrow(tempSamples)),
                                                 stringsAsFactors=FALSE)
                    g=1
                    if (screen$alphaBeta[b] == 0) { #calculator function 1 
                        for (y in 1:nrow(tempSamples)) { #iterate through each sample 
                            screen$value[b] <- as.numeric((exp((screen$maSlope[b]*log(tempSamples$Hardness[y]))+screen$mbIntercept[b])*screen$conversionFactor[b])/1000) #calculate criteria
                            aquatic_screen[g,] <- c(tempSamples$Date_Time[y],
                                                    tempSamples$Samp_No[y],
                                                    screen$Designated_Use[b],
                                                    tempSamples$Sp_Layer[y],
                                                    screen$ScreenType[b], 
                                                    tempSamples$Lat[y],
                                                    tempSamples$Lon[y],
                                                    screen$NAME[b],
                                                    screen$Sample_Type[b],
                                                    screen$variable[b],
                                                    as.numeric(screen$value[b]), 
                                                    as.numeric(tempSamples$conc[y]), 
                                                    tempSamples$variable[y]) #collect criteria and sample value (for screen eval)
                            
                            aquatic_screen[, c(11:12)] <- sapply(aquatic_screen[, c(11:12)], as.numeric)
                            g=g+1
                        }
                    } else if (screen$alphaBeta[b] == 1) { #calculator function 2 
                        for (z in 1:nrow(tempSamples)) { #iterate through each sample
                            screen$value[b] <- as.numeric((exp((screen$maSlope[b]*log(tempSamples$Hardness[z])+screen$mbIntercept[b]))*(screen$alpha[b]-(log(tempSamples$Hardness[z])*screen$beta[b])))/1000) #calculate criteria
                            aquatic_screen[g,] <- c(tempSamples$Date_Time[z],
                                                    tempSamples$Samp_No[z],
                                                    screen$Designated_Use[b],
                                                    tempSamples$Sp_Layer[z],
                                                    screen$ScreenType[b], 
                                                    tempSamples$Lat[z],
                                                    tempSamples$Lon[z],
                                                    screen$NAME[b],
                                                    screen$Sample_Type[b],
                                                    screen$variable[b],
                                                    as.numeric(screen$value[b]), 
                                                    as.numeric(tempSamples$conc[z]), 
                                                    tempSamples$variable[z]) #collect criteria and sample value (for screen eval)
                            
                            aquatic_screen[, c(11:12)] <- sapply(aquatic_screen[, c(11:12)], as.numeric)
                            g=g+1
                        }
                    } else {
                        cat("Something went wrong with the hardness calculator.", "The error occured calculating the screening criteria for",screen$Sample_Type[b],
                            screen$variable[b], "using the",screen$ScreenType[b], "screen for",screen$NAME[b])
                    }
                    aquatic_screen_cleaned <- filter(aquatic_screen, CalcValue >= 0 & SampleValue >= 0 & CritMetal != "") #remove empty rows in data.frame
                    n_screened <- nrow(aquatic_screen_cleaned) #count the number of samples that are screened
                    n_screened[is.null(n_screened)] <- -50
                    if (n_screened > 0) {
                        metal_vector_exceedances <- aquatic_screen_cleaned[which(aquatic_screen_cleaned$SampleValue > aquatic_screen_cleaned$CalcValue),] #filter criteria with exceedances
                        metal_exceedance_count <- nrow(metal_vector_exceedances) #count exceedances
                        m=m+1
                        
                        for (f in 1:nrow(aquatic_screen_cleaned)) {
                            n=n+1
                            Samplemarkerlayer[n,] <- aquatic_screen_cleaned[f,]
                            
                        }
                        
                        # if (input$Categories==TRUE)  {
                        nCategories <- (UniqueObs[,c(-1,-ncol(UniqueObs))])
                        screenvars1 <- c(screen$Designated_Use[b],
                                         screen$ScreenType[b],
                                         screen$NAME[b])
                        screenvars2 <- NULL
                        for (x in 1:length(GroupCategories[-length(GroupCategories)])) {
                            screenvars2[x] <- if (length(GroupCategories[-length(GroupCategories)])>1) {nCategories[i,x]} else {nCategories[i]}
                        }
                        screenvars3 <- c(screen$variable[b],
                                         metal_exceedance_count,
                                         n_screened)
                        
                        screenvarTot <- c(screenvars1,screenvars2,screenvars3)
                        output_screen[m,] <- screenvarTot
                        # } else {
                        # output_screen[m,] <- c(screen$Designated_Use[b],
                        #                        screen$ScreenType[b],
                        #                        screen$NAME[b],
                        #                        screen$Sample_Type[b],
                        #                        screen$variable[b],
                        #                        metal_exceedance_count,
                        #                        n_screened)
                        # }
                    }
                } else {
                    if (!all(is.na(tempSamples$conc))) { #distinguishes between a non-detect sample and no sample
                        metal_vector_nonas <- tempSamples[!is.na(tempSamples$conc),]#remove NAs
                        num_metal_samples <- nrow(metal_vector_nonas) #count the number of samples that are screened
                        if(is.null(num_metal_samples)){num_metal_samples <- -50}
                        if (num_metal_samples > 0) {
                            metal_vector_exceedances <- metal_vector_nonas[which(metal_vector_nonas$conc>screen$value[b]),] #filter criteria with exceedances
                            metal_exceedance_count <- nrow(metal_vector_exceedances) #count exceedances
                            m=m+1
                            
                            for (t in 1:num_metal_samples) {
                                n=n+1
                                Samplemarkerlayer[n,] <-  c(metal_vector_nonas$Date_Time[t], 
                                                            metal_vector_nonas$Samp_No[t],
                                                            screen$Designated_Use[b], 
                                                            metal_vector_nonas$Sp_Layer[t],
                                                            screen$ScreenType[b], 
                                                            metal_vector_nonas$Lat[t],
                                                            metal_vector_nonas$Lon[t],
                                                            metal_vector_nonas$NAME[t], 
                                                            metal_vector_nonas$Sample_Type[t],
                                                            screen$variable[b],
                                                            as.numeric(screen$value[b]),
                                                            as.numeric(metal_vector_nonas$conc[t]),
                                                            unique(tempSamples$variable))   
                            }
                            #if (input$Categories==TRUE)  {
                            nCategories <- (UniqueObs[,c(-1,-ncol(UniqueObs))])
                            screenvars1 <- c(screen$Designated_Use[b],
                                             screen$ScreenType[b],
                                             screen$NAME[b])
                            screenvars2 <- NULL
                            
                            for (x in 1:length(GroupCategories[-length(GroupCategories)])) {
                                screenvars2[x] <- if (length(GroupCategories[-length(GroupCategories)])>1) {nCategories[i,x]} else {nCategories[i]}
                            }
                            screenvars3 <- c(screen$variable[b],
                                             metal_exceedance_count,
                                             num_metal_samples)
                            
                            screenvarTot <- c(screenvars1,screenvars2,screenvars3)
                            output_screen[m,] <- screenvarTot
                            # } else {
                            # output_screen[m,] <- c(screen$Designated_Use[b],
                            #                        screen$ScreenType[b],
                            #                        screen$NAME[b],
                            #                        screen$Sample_Type[b],
                            #                        screen$variable[b],
                            #                        metal_exceedance_count,
                            #                        num_metal_samples)
                            #}
                        }
                    }
                }
            }
        } else {
            cat(UniqueObs$Sample_Type[i], 
                UniqueObs$variable, 
                UniqueObs$NAME[i], 
                file="echoFile.txt", append=TRUE)
        }
    }

    #start_time <- Sys.time()
    #sleep_for_a_minute()
    #end_time <- Sys.time()
    
    #end_time - start_time
output_screen <- filter(output_screen, ScreenType!="")
output_screen$Times_Exceeded <- as.numeric(output_screen$Times_Exceeded)
output_screen_Exceeded <- filter(output_screen, Times_Exceeded > 0)
#write.csv(WQCritAll,file="WQCritAll.csv", row.names=FALSE)
write.csv(output_screen,file="Reload1_file_nonshinyapp.csv", row.names=FALSE)

if (exists("Samplemarkerlayer")){ 
    samplemarkers_screen <- filter(Samplemarkerlayer, ScreenType!="") %>%
        mutate(SampleValue = as.numeric(SampleValue),
               CalcValue = as.numeric(CalcValue),
               Difference = SampleValue/CalcValue, 
               Type = ifelse(Difference < 1,"NotExceeded","Exceeded"),
               Lat = as.numeric(Lat),
               Lon = as.numeric(Lon),
               #Date_Time = as.POSIXct(Date_Time,format = '%m/%d/%Y %H:%M'), #,usetz = FALSE
               Date_Time = format(as.POSIXct(Date_Time,format = '%m/%d/%Y %H:%M'),format='%Y-%m-%d')) # %H:%M
        write.csv(samplemarkers_screen,file="Samplelatlondiferences.csv",row.names = FALSE)
}
