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

#This tool was created by Brian Avant
#The purpose of this tool is to screen GKM related datasets containing metals concentrations in the water column against water quality standards for specific areas.

# 
#Read in screening critiera and sample data
##NOTE: It is very important that you preserve numeric precision in your input files! To do this make sure cells with numerical values are General format.
setwd("C:/Users/bavant/Dropbox/WQScreen") #work /Git/WQScreen
#setwd("C:/Users/Brian/Dropbox/WQScreen") #laptop wd
WQCritSS <-  read.table("WQCriteriaTot.txt",sep="\t",skip =0, header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)
WQCritHardness <- read.table("WQCriteriawHardness.txt",sep="\t",skip =0, header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)
#Reformat WQ Screening Criteria
WQCritmelt <- melt(WQCritSS,id.vars=c("Designated_Use","ScreenType","Spatial_Bound","Spatial_Type","Sample_Type"))
WQCritSS_clean <- WQCritmelt[complete.cases(WQCritmelt),]
WQCritSS_clean$variable <- as.character(WQCritSS_clean$variable, stringsAsFactors = FALSE)
namevector <- c("maSlope","mbIntercept", "alphaBeta", "conversionFactor", "alpha", "beta")
WQCritSS_clean[,namevector] <- NA
WQCritAll <- rbind(WQCritSS_clean,WQCritHardness)
#create output data.frames
rows <- nrow(WQCritmelt)
output_screen <- data.frame(Designated_Use = character(rows), 
                            ScreenType = character(rows), 
                            Spatial_Bound = character(rows), 
                            River = character(rows), 
                            Time_Period = character(rows), 
                            Sample_Type = character(rows), 
                            Metal = character(rows), 
                            Times_Exceeded = numeric(rows), 
                            Number_Screened = numeric(rows), 
                            stringsAsFactors=FALSE)

###Dropdown menus
Authoritylist <- c("Select",unique(WQCritSS_clean$Spatial_Type))
Contaminantlist <- c("Select",unique(WQCritSS_clean$variable))

Sampletype <- c("Select",unique(WQCritSS_clean$Sample_Type))
Criteralist <- c("Select",unique(WQCritSS_clean$Designated_Use)) #need to make reactive so they update/limit options based on criteria

###latlong Conversion Function #######################################################
latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE) #transparent
  
  StateIDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=StateIDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  states_indices <- over(pointsSP, states_sp)
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[states_indices]
}

latlong2tribe <- function(pointsDF) {
    # Prepare SpatialPolygons object with one SpatialPolygon
    #tribes <- fromJSON("Tribal_Lands.json")
    tribesSHP <- readOGR(dsn=".", layer="cb_2015_us_aiannh_500k",GDAL1_integer64_policy = TRUE)
    tribes <- map(tribesSHP, fill=TRUE, col="transparent", plot=FALSE)
    TribesIDs <- sapply(strsplit(tribes$names, ":"), function(x) x[1])
    
    tribes_sp <- map2SpatialPolygons(tribes, IDs=TribesIDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
    # Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(pointsDF, 
                              proj4string=CRS("+proj=longlat +datum=WGS84"))
    # Use 'over' to get _indices_ of the Polygons object containing each point 
    tribes_indices <- over(pointsSP, tribes_sp)
    # Return the state names of the Polygons object containing each point
    tribeNames <- sapply(tribes_sp@polygons, function(x) x@ID)
    tribeNames[tribes_indices]
    
}

latlong2region <- function(pointsDF) {
    # Prepare SpatialPolygons object with one SpatialPolygon
    regionsSHP <- readOGR(dsn=".", layer="EPA_Regions",GDAL1_integer64_policy = TRUE)
    regions <- map(regionsSHP, fill=TRUE, col="transparent", plot=FALSE)
    
    #EPARegions <- fromJSON("EPA_Regions_simple.json")
    #EPARegions2 <- readOGR(dsn = "EPA_Regions.json",layer = "GeometryCollection")
    RegionsIDs <- sapply(strsplit(regions$names, ":"), function(x) x[1])
    
    regions_sp <- map2SpatialPolygons(regions, IDs=RegionsIDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
    # Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(pointsDF, 
                              proj4string=CRS("+proj=longlat +datum=WGS84"))
    # Use 'over' to get _indices_ of the Polygons object containing each point 
    regions_indices <- over(pointsSP, regions_sp)
    # Return the state names of the Polygons object containing each point
    regionNames <- sapply(regions_sp@polygons, function(x) x@ID)
    regionNames[regions_indices]
    
}
####################################################################################
i=0
j=0
b=1
m=0
y=1
z=1

ui <- fluidPage(
  navbarPage("WQ",id="nav",
             tabPanel("Inputs",
                      fluidRow(column(4,
                                      wellPanel(fileInput(inputId = "Samples", label = "Import Samples File"),
                                                radioButtons(inputId = "Spatialdist", label ="Location Input Type",c("By Name"="Name","By Lat Lon"="LatLon")),
                                                checkboxInput(inputId = "checked", 
                                                              label = "Include contaminants that were screened but did not exceed Authority",
                                                              value = FALSE),
                                                actionButton(inputId = "Click", label = "Screen Samples")))),
                      fluidRow(column(6, textOutput(outputId="screenprogress"),
                                      textOutput(outputId="metal")
                      ))),
             tabPanel("Interactive Map", id="Map", 
                      fluidRow(column(10,
                                      leafletOutput("map",width="100%",height="600px"),
                                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                    draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                                    width = 330, height = "auto",
                                                    
                                                    h2("Screening Metrics"),
                                                    htmlOutput("Bound_selector"),
                                                    htmlOutput("Metal_selector"),
                                                    htmlOutput("Type_selector"),
                                                    htmlOutput("Criteria_selector"),
                                                    checkboxInput(inputId = "checkedsamples", 
                                                                  label = "Show Screened Samples",
                                                                  value = FALSE)
                                                    #selectInput("Authority", "Authority", choices = unique(as.character(WQCritSS_clean$Spatial_Type)),selected ="States")
                                                    #selectInput("Contaminant", "Contaminant",uiOutput("secondSelection")),
                                                    #selectInput("Contaminant", "Contaminant", Contaminantlist),
                                                    #selectInput("Sampletype", "Sample Type", Sampletype)
                                                    #selectInput("Criteria", "Criteria", Criteralist)
                                      ))
                               #column(10,
                                      #leafletOutput("map2",width="100%",height="400px")
                               #)
                      )
             ),         
             tabPanel("Tables",
                      h2('Water Quality Screen Results'),
                      dataTableOutput("Results")),
             tabPanel("Figures",
                      h2('Pivot Tables'),
                      rpivotTableOutput("Pivot1")
             )
  )
)

server <- function(input, output) {
######################### set up for interactive map############v################
    
    output$Bound_selector <- renderUI({
        
        selectInput(
            inputId = "Authority", 
            label = "Authority",
            choices = unique(as.character(WQCritSS_clean$Spatial_Type)),
            selected = "States")
        
    })
    
    SelectedAuthority <- reactive({
        input$Authority
        
    })
    
    output$Metal_selector <- renderUI({
        
        available1 <- WQCritSS_clean[WQCritSS_clean$Spatial_Type == input$Authority,"variable"]
        Spatial1 <- WQCritSS_clean[WQCritSS_clean$Spatial_Type == input$Authority,"Spatial_Bound"]
        selectInput(
            inputId = "Contaminant", 
            label = "Contaminant",
            choices = unique(available1),
            selected = unique(available1)[1])
        
    })
    
    output$Type_selector <- renderUI({
        
        available2 <- WQCritSS_clean[WQCritSS_clean$Spatial_Type == input$Authority & WQCritSS_clean$variable == input$Contaminant,"Sample_Type"]
        
        selectInput(
            inputId = "Sampletype", 
            label = "Sample Type",
            choices = unique(available2),
            selected = unique(available2)[1])
        
    })
    
    output$Criteria_selector <- renderUI({
        
        available3 <- WQCritSS_clean[WQCritSS_clean$Spatial_Type == input$Authority & WQCritSS_clean$variable == input$Contaminant & WQCritSS_clean$Sample_Type == input$Sampletype,"ScreenType"]
        
        selectInput(
            inputId = "Criteria", 
            label = "Criteria",
            choices = unique(available3),
            selected = unique(available3)[1])
        
    })
    
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = -98.35, lat = 39.5,  zoom = 4)
    })
    
    # observe({
    #     Authority <- SelectedAuthority()
    #     validate(need(nrow(Authority) > 0, FALSE))
    #     if (Authority== "Tribes") {
    #         leafletProxy("map", data = tribes) %>%
    #             clearShapes() %>%
    #             addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
    #                         opacity = 1.0, fillOpacity = 0.5)
    #     } else if (Authority== "States"){
    #         leafletProxy("map", data = states) %>%
    #             clearShapes() %>%
    #             addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
    #                         opacity = 1.0, fillOpacity = 0.5)
    #     } else if (Authority== "EPA Regions"){
    #         leafletProxy("map", data = regions) %>%
    #             clearShapes() %>%
    #             addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
    #                         opacity = 1.0, fillOpacity = 0.5)
    #     } else {}
    # })  
    
     observe({

        Sp_layer <- if(is.null(input$Authority)) {
            return(NULL)
            } else if (input$Authority == "Tribes"){
            tribes
            } else if (input$Authority == "States"){
                states
            } else if (input$Authority == "EPA Regions"){
                regions
                } else {}
                

        data1 <- Sp_layer
        #data1 <- states
        #data2 <- filter(data1,str_to_title(names) == unique(Spatial1))
        
        Cont_Crit <- input$Contaminant
        Type_Crit <- input$Sampletype
        Crit_Crit <- input$Criteria
        #bins <- c(0,(data2$value)*0.25,(data1$value)*0.50,(data1$value)*0.75,Inf)
        #pal <- colorBin("YlOrRd", domain = data1$value, bins = bins)
        
         leafletProxy("map", data = data1) %>%
             clearShapes() %>%
             addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5)
             # addPolygons(fillColor = ~pal(value),
             #             weight = 2,
             #             opacity = 1,
             #             color = "white",
             #             dashArray = "3",
             #             fillOpacity = 0.7)

    })
    
  
  filedata <- reactive({
    infile <- input$Samples
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
      
    }
    read.table(infile$datapath,sep="\t",skip =0, header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)
  })
  
  observeEvent(input$Click, {
    df <- filedata()
    
    if (input$Spatialdist == "LatLon") { #lat lon version
        
        #Sample Sites
        samplemarkers <- select(df, c(Lon,Lat,Samp_No))
        
        #Collect relevant spatial boundaries from sample lat lon
        samplecoords <- select(df, c(Lon,Lat))
        Spatial_Boundstate <- str_to_title(latlong2state(samplecoords)) 
        Spatial_Boundregion <- str_to_title(latlong2region(samplecoords))
        Spatial_Boundtribe <- str_to_title(latlong2tribe(samplecoords))
        
        
        ## add States column to sample data and remove NAs
        ObsSpatial_BoundsStatena <- add_column(df, Spatial_Boundstate, .after = 1) 
        #Spatial_Type_col_State <- add_column(ObsSpatial_BoundsStatena, rep("States",length(ObsSpatial_BoundsStatena)), .after = 2) 
        ObsSpatial_BoundsState <- complete.cases(ObsSpatial_BoundsStatena[,2])
        ObsAllSpatial_BoundsState <- ObsSpatial_BoundsStatena[ObsSpatial_BoundsState, ]
        colnames(ObsAllSpatial_BoundsState)[2] <- "Spatial_Bound"
        
        ## add EPA Region column to sample data and remove NAs
        Region_samps <- df[1:length(Spatial_Boundregion),]
        ObsSpatial_BoundsRegionna <- add_column(Region_samps, Spatial_Boundregion, .after = 1) 
        #Spatial_Type_col_Region <- add_column(ObsSpatial_BoundsRegionna, rep("EPA Regions",length(ObsSpatial_BoundsRegionna)), .after = 2) 
        ObsSpatial_BoundsRegion <- complete.cases(ObsSpatial_BoundsRegionna[,2])
        ObsAllSpatial_BoundsRegion <- ObsSpatial_BoundsRegionna[ObsSpatial_BoundsRegion, ]
        colnames(ObsAllSpatial_BoundsRegion)[2] <- "Spatial_Bound"
        
        ## add Tribe column to sample data and remove NAs
        Tribe_samps <- df[1:length(Spatial_Boundtribe),]
        ObsSpatial_BoundsTribena <- add_column(Tribe_samps, Spatial_Boundtribe, .after = 1)
        #Spatial_Type_col_Tribe <- add_column(ObsSpatial_BoundsTribena, rep("Tribes",length(ObsSpatial_BoundsTribena)), .after = 2) 
        ObsSpatial_BoundsTribe <- complete.cases(ObsSpatial_BoundsTribena[,2])
        ObsAllSpatial_BoundsTribe <- ObsSpatial_BoundsTribena[ObsSpatial_BoundsTribe, ]
        colnames(ObsAllSpatial_BoundsTribe)[2] <- "Spatial_Bound"
        
        #append all sample boundaries to one df
        ObsAllSpatial_Bounds <- rbind(ObsAllSpatial_BoundsState,ObsAllSpatial_BoundsRegion,ObsAllSpatial_BoundsTribe)
        index1 <- 1 + which( colnames(ObsAllSpatial_Bounds)=="Hardness" )
        
    } else {
      Tribes <- df[complete.cases(df[,3]),]
      Tribes2 <- df[complete.cases(df[,4]),]
      colnames(Tribes) [3] <- "Spatial_Bound"
      colnames(Tribes2) [4] <- "Spatial_Bound"
      colnames(df) [2] <- "Spatial_Bound"
      ObsAllSpatial_Bounds <- rbind(df[,-c(3:4)],Tribes[,c(-2,-4)],Tribes2[,-c(2:3)])
      
      index1 <- 1 + which( colnames(ObsAllSpatial_Bounds)=="Hardness" )
    }
    #Cap hardness values based on specific criteria
    obsCapped <- within(ObsAllSpatial_Bounds, Hardness[Hardness>400] <- 400) #Maximum hardness of 400 mg/L for most criteria in the region
    
    #This is the main function of the tool. For each sample the applicable screening criteria are identified and used to 
    ## determine the number of times a WQ criteria has been exceeded for a specific screen.
    UniqueObs <- unique(obsCapped[c("Spatial_Bound","Sample_Type","River","Time_Period")]) 
    
    for (i in 1:nrow(UniqueObs)) { #loops through each sample by unique combinations of region and conc type(row)
        print(UniqueObs[i,])
      ############ Figure out how to display console messages in the Shiny app to let users know progress of code
      #currentSpatial_Bound <- UniqueObs[i,1]
      #currentSampleType <- UniqueObs[i,2]
      #currentRiver <- UniqueObs[i,3]
      #currentTimePeriod <- UniqueObs[i,4]
      #message <- paste(currentSpatial_Bound, currentSampleType, currentRiver,currentTimePeriod,sep = " ")
      
      #rv1 <- reactiveValues(data= message)
      
      
      
      for (j in index1:ncol(obsCapped)){ #loops through each metal
        tempSamples <- filter(obsCapped, Spatial_Bound==UniqueObs[i,1], 
                              Sample_Type==UniqueObs[i,2], 
                              River==UniqueObs[i,3], 
                              Time_Period==UniqueObs[i,4]) #subset observed data by unique combination
        
        
        print(colnames(tempSamples[j]))
        
        
        if (UniqueObs[i,1]=="New Mexico" & 
            UniqueObs[i,2]=="Total" & 
            colnames(tempSamples[j])=="Aluminum") { #New Mexico hardness limit for total Al = 220 mg/L
          tempSamples <- within(tempSamples, Hardness[Hardness>220] <- 220)
        }
        hardness <- data.frame("Hardness" = tempSamples$Hardness, 
                               "Conc" = tempSamples[j], 
                               "ObsMetal" = colnames(tempSamples[j]), 
                               stringsAsFactors=FALSE) 
        screen <- filter(WQCritAll, 
                         Spatial_Bound==UniqueObs$Spatial_Bound[i], 
                         Sample_Type==UniqueObs$Sample_Type[i],
                         variable==colnames(tempSamples[j])) #iteratively queries WQ criteria based on sample data (sample & metal)
        if (length(screen$value) > 0){
          for (b in 1:length(screen$ScreenType)) { #loop through matching screens 
            if (!is.na(screen$maSlope[b]==TRUE)) { #find screens that need to be calculated based on hardness
              aquatic_screen <- data.frame(Designated_Use = character(nrow(tempSamples)), 
                                           ScreenType = character(nrow(tempSamples)),
                                           Spatial_Bound = character(nrow(tempSamples)),
                                           Sample_Type = character(nrow(tempSamples)),
                                           CritMetal = character(nrow(tempSamples)),
                                           CalcValue = numeric(nrow(tempSamples)),
                                           SampleValue = numeric(nrow(tempSamples)),
                                           ObsMetal = character(nrow(tempSamples)),
                                           stringsAsFactors=FALSE)
              g=1
              if (screen$alphaBeta[b] == 0) { #calculator function 1 
                for (y in 1:nrow(hardness)) { #iterate through each sample 
                  screen$value[b] <- as.numeric((exp((screen$maSlope[b]*log(hardness$Hardness[y]))+screen$mbIntercept[b])*screen$conversionFactor[b])/1000) #calculate criteria
                  aquatic_screen[g,] <- c(screen$Designated_Use[b], 
                                          screen$ScreenType[b], 
                                          screen$Spatial_Bound[b],
                                          screen$Sample_Type[b],
                                          screen$variable[b],
                                          screen$value[b], 
                                          hardness[y,2], 
                                          hardness[y,3]) #collect criteria and sample value (for screen eval)
                  aquatic_screen[, c(6:7)] <- sapply(aquatic_screen[, c(6:7)], as.numeric)
                  g=g+1
                }
              } else if (screen$alphaBeta[b] == 1) { #calculator function 2 
                for (z in 1:nrow(hardness)) { #iterate through each sample
                  screen$value[b] <- as.numeric((exp((screen$maSlope[b]*log(hardness$Hardness[z])+screen$mbIntercept[b]))*(screen$alpha[b]-(log(hardness$Hardness[z])*screen$beta[b])))/1000) #calculate criteria
                  aquatic_screen[g,] <- c(screen$Designated_Use[b], 
                                          screen$ScreenType[b], 
                                          screen$Spatial_Bound[b],
                                          screen$Sample_Type[b],
                                          screen$variable[b],
                                          as.numeric(screen$value[b]), 
                                          as.numeric(hardness[z,2]), 
                                          hardness[z,3]) #collect criteria and sample value (for screen eval)
                  aquatic_screen[, c(6:7)] <- sapply(aquatic_screen[, c(6:7)], as.numeric)
                  g=g+1
                }
              } else {
                cat("Something went wrong with the hardness calculator.", "The error occured calculating the screening criteria for",screen$Sample_Type[b],
                    screen$variable[b], "using the",screen$ScreenType[b], "screen for",screen$Spatial_Bound[b])
              }
              aquatic_screen_cleaned <- filter(aquatic_screen, CalcValue >= 0, SampleValue >= 0) #remove empty rows in data.frame
              n_screened <- nrow(aquatic_screen_cleaned) #count the number of samples that are screened
              n_screened[is.null(n_screened)] <- -500
              if (n_screened > 0) {
                metal_vector_exceedances <- which(aquatic_screen_cleaned$SampleValue > aquatic_screen_cleaned$CalcValue) #filter criteria with exceedances
                metal_exceedance_count <- length(metal_vector_exceedances) #count exceedances
                m=m+1
                output_screen[m,] <- c(screen$Designated_Use[b], 
                                       screen$ScreenType[b], 
                                       screen$Spatial_Bound[b], 
                                       UniqueObs[i,3], 
                                       UniqueObs[i,4], 
                                       screen$Sample_Type[b],
                                       screen$variable[b],
                                       metal_exceedance_count, 
                                       n_screened)
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
                  output_screen[m,] <- c(screen$Designated_Use[b], 
                                         screen$ScreenType[b], 
                                         screen$Spatial_Bound[b], 
                                         UniqueObs[i,3], 
                                         UniqueObs[i,4], 
                                         screen$Sample_Type[b],
                                         screen$variable[b],
                                         metal_exceedance_count, 
                                         num_metal_samples) 
                }
              }
            }
          }
        } else {
          cat(UniqueObs$Sample_Type[i], 
              colnames(tempSamples[j]), 
              UniqueObs$Spatial_Bound[i], 
              file="echoFile.txt", append=TRUE)
        }
      }
    }
    output_screen <- filter(output_screen, ScreenType!="")
    output_screen$Times_Exceeded <- as.numeric(output_screen$Times_Exceeded)
    output_screen_Exceeded <- filter(output_screen, Times_Exceeded > 0)
    
    write.csv(WQCritAll,file="WQCritAll.csv")
    
    output$map <- renderLeaflet({ ## Open sample markers in map
        # if (input$checkedsamples==FALSE)  {
        #     leaflet("map", data = data1) %>%
        #         clearShapes() %>%
        #         addTiles() %>%
        #         addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
        #                     opacity = 1.0, fillOpacity = 0.5)
        #  } else if (input$checked==TRUE) {
             leaflet(data = samplemarkers) %>%
                 addTiles() %>%
                 addMarkers(~Lon, ~Lat,popup = ~as.character(Samp_No))
         #}
    })
    
     output$Results = renderDataTable({
      if (input$checked==FALSE)  {output_screen
      } else {
        if (input$checked==TRUE) {output_screen_Exceeded}
      }
    })
     
    output$Pivot1 = renderRpivotTable({
      rpivotTable(data=output_screen, rows = c("Designated_Use","Time_Period"),cols = c("Metal","River","Spatial_Bound"), rendererName = "Bar Chart",aggregatorName = "Sum over Sum", vals = c("Times_Exceeded","Number_Screened"))
    })
    output$screenprogress <- renderPrint({
      message("Screen Complete")
      cat("Screen Complete")
      #message(samplemarkers)
      
      
      })
    write.csv(samplemarkers,file="SamplelatlonOutput.csv",row.names = FALSE)
  })
  
}

shinyApp(ui= ui, server = server)