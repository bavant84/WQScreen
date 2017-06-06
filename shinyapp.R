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
library(dataRetrieval)
library(rmapshaper)
library(DT)
library(xlsx) 

## This tool was created by Brian Avant
## The purpose of this tool is to screen GKM related datasets containing metals concentrations in the water column against water quality standards for specific areas.

## Read in screening critiera and sample data
## NOTE: It is very important that you preserve numeric precision in your input files! To do this make sure cells with numerical values are General format.
#setwd("C:/Users/bavant/Dropbox/WQScreen") #work /Git/WQScreen
setwd("C:/Users/Brian/Dropbox/WQScreen") #laptop wd
WQCritSS <-  read.table("WQCriteriaTot.txt",sep="\t",skip =0, header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)
WQCritHardness <- read.table("WQCriteriawHardness.txt",sep="\t",skip =0, header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)
## Reformat WQ Screening Criteria
WQCritmelt <- melt(WQCritSS,id.vars=c("Designated_Use","ScreenType","NAME","Spatial_Type","Sample_Type"))
WQCritSS_clean <- WQCritmelt[complete.cases(WQCritmelt),]
WQCritSS_clean$variable <- as.character(WQCritSS_clean$variable, stringsAsFactors = FALSE)
namevector <- c("maSlope","mbIntercept", "alphaBeta", "conversionFactor", "alpha", "beta")
WQCritSS_clean[,namevector] <- NA
WQCritAll <- rbind(WQCritSS_clean,WQCritHardness)

## Create Output data.frames
rows <- nrow(WQCritmelt)

Samplemarkerlayer <- data.frame(Sample_No = character(rows*10),
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
statesJSON <- readOGR("selected_states.geojson", "OGRGeoJSON", verbose = FALSE) #selected_
states <- map(statesJSON, fill=TRUE, col="transparent", plot=FALSE)
StateIDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
states_sp <- map2SpatialPolygons(states, IDs=StateIDs,
                                 proj4string=CRS("+proj=longlat +datum=WGS84"))

tribesJSON <- readOGR("tribes.geojson", "OGRGeoJSON", verbose = FALSE)
tribesmap <- map(tribesJSON, fill=TRUE, col="transparent", plot=FALSE)
TribesIDs <- sapply(strsplit(tribesmap$names, ":"), function(x) x[1])
tribes_sp <- map2SpatialPolygons(tribesmap, IDs=TribesIDs,
                                 proj4string=CRS("+proj=longlat +datum=WGS84"))

regionsJSON <- readOGR("EPA_regions.geojson", "OGRGeoJSON", verbose = FALSE)
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
i=0
j=0
b=1
m=0
n=0
y=1
z=1
t=1
f=1
g=0

ui <- fluidPage(
    navbarPage("WQ",id="nav",
               tabPanel("Inputs",
                        fluidRow(column(4,
                                        wellPanel(fileInput(inputId = "Samples", label = h3("Import Sample File")),
                                                  radioButtons(inputId = "Spatialdist", label =h4("Location Input Type"),
                                                               c("By Name"="Name","By Lat Lon"="LatLon")),
                                                  checkboxInput(inputId = "Categories", 
                                                                label = "Group by Spatial or Temporal Categories",
                                                                value = FALSE),
                                                  checkboxInput(inputId = "checked", 
                                                                label = "Include contaminants that were screened but did not exceed Authority",
                                                                value = FALSE),
                                                  actionButton(inputId = "Click", label = "Screen Samples"))),
                                 column(5,wellPanel(dateRangeInput(inputId = "WQXDates", 
                                                                   label = h4("Date Range for WQX Data")), 
                                                    selectInput(inputId = "selectstate", label = h4("Select State"),
                                                                choices = statesJSON$NAME, 
                                                                selected = statesJSON$NAME[1]),
                                                    selectInput(inputId = "selectcontaminant", label = h4("Select Contaminant"),
                                                                choices = list("Aluminum" = 1, "Arsenic" = 2, "Lead" = 3), 
                                                                selected = 1),
                                                    selectInput(inputId = "selecttype", label = h4("Select Type"),
                                                                choices = list("Total" = 1, "Dissolved" = 2), 
                                                                selected = 1),
                                                    selectInput(inputId = "selectunits", label = h4("Select Units"),
                                                                choices = list("mg/l", "ug/l")),
                                                    actionButton(inputId = "ClickWQX", label = "Download Data"))),
                                 column(3,fileInput(inputId = "Reload", label = h3("Reload Results File")),
                                        #radioButtons(inputId = "Spatialdist", label =h4("Location Input Type"),c("By Name"="Name","By Lat Lon"="LatLon")),
                                        actionButton(inputId = "reClick", label = "Load Screened Samples"))),
                        fluidRow(column(6, textOutput(outputId="screenprogress"),
                                        textOutput(outputId="metal")
                        ))),
               tabPanel("Interactive Map", id="Map", 
                        div(class="outer",
                            tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")
                            ),
                            leafletOutput("map",width="100%",height="100%"),
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                          width = 330, height = "auto",
                                          h2("Screening Metrics"),
                                          htmlOutput("Bound_selector"),
                                          wellPanel(htmlOutput("Metal_selector"),
                                                    htmlOutput("Type_selector"),
                                                    htmlOutput("Criteria_selector"),
                                                    actionButton(inputId = "Clickmap", label = "Get Criteria")),
                                          conditionalPanel( condition = "output.Samplenrows",
                                                            checkboxInput("ImportedSamples", "Imported Samples")),
                                          conditionalPanel( condition = "output.WQXnrows",
                                                            checkboxInput("WQXSamples", "WQX/STORET Samples"))
                            ))
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
############################## set up for interactive map ############################
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = -98.35, lat = 39.5,  zoom = 4)
    })
    
    output$Bound_selector <- renderUI({
        selectInput(
            inputId = "Authority", 
            label = "Authority",
            choices = unique(as.character(WQCritAll$Spatial_Type)),
            selected = "States")
    })
    
    output$Metal_selector <- renderUI({
        available1 <- WQCritAll[WQCritAll$Spatial_Type == input$Authority,"variable"]
        selectInput(
            inputId = "Contaminant", 
            label = "Contaminant",
            choices = unique(available1),
            selected = unique(available1)[1])
        #unique(available1)[1]
    })
    
    output$Type_selector <- renderUI({
        available2 <- WQCritAll[WQCritAll$Spatial_Type == input$Authority & WQCritAll$variable == input$Contaminant,"Sample_Type"]
        selectInput(
            inputId = "Sampletype", 
            label = "Sample Type",
            choices = unique(available2),
            selected = unique(available2)[1])
        #unique(available2)[1]
    })
    
    output$Criteria_selector <- renderUI({
        available3 <- WQCritAll[WQCritAll$Spatial_Type == input$Authority & WQCritAll$variable == input$Contaminant & WQCritAll$Sample_Type == input$Sampletype,"ScreenType"]
        selectInput(
            inputId = "Criteria", 
            label = "Criteria",
            choices = unique(available3),
            selected = unique(available3)[1])
    })
    
    Authority_Layer <- reactive({
        req(input$Authority)
        if (input$Authority == "Tribes"){
            tribesJSON
        } else if (input$Authority == "States"){
            statesJSON
        } else if (input$Authority == "Regions"){
            regionsJSON
        } else {}
    })
    
    observe({
        selected_layer <- Authority_Layer()
        
        if(is.null(selected_layer)) {
            print("Nothing selected")
            leafletProxy("map") #%>% clearMarkers()
        }
        else{
            leafletProxy("map",data = selected_layer) %>%
                clearShapes() %>%
                addPolygons(color = "#A9A9A9",
                            fillColor = "#d3d3d3", 
                            stroke = TRUE, 
                            smoothFactor = 0.5,
                            opacity = 1.0, fillOpacity = 0.5, weight = 1) #%>%
            #setView(lng = -98.35, lat = 39.5,  zoom = 4) 
        }
    })
################################################################################# 
    filedata <- reactive({
        req(input$Samples)
        infile <- input$Samples
        read.table(infile$datapath,sep="\t",skip =0, header = TRUE,na.strings = "NA",stringsAsFactors=FALSE)
    })
    
    observeEvent(input$Click, {
        df <- filedata()
        
        if (input$Spatialdist == "LatLon") { #lat lon version
            
            ## Sample Sites
            samplemarkers <- select(df, c(Lon,Lat,Samp_No))
            #write.csv(df,"samplemarkers.csv")
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
            
        } else {
            df2 <- add_column(df, Sp_Layer = "State", Lat = NA, Lon = NA, .after = 2)
            Tribes_col <- df2[complete.cases(df2$Tribe),]
            if (nrow(Tribes_col) > 0) {Tribes_col$Sp_Layer <- "Tribes"}
            Tribes_col2 <- df2[complete.cases(df2$Secondary_Tribe),]
            if (nrow(Tribes_col2) > 0) {Tribes_col2$Sp_Layer <- "Tribes"}
            names(Tribes_col)[names(Tribes_col)=="Tribe"] <- "NAME"
            names(Tribes_col2)[names(Tribes_col2)=="Secondary_Tribe"] <- "NAME"
            names(df2)[names(df2)=="Region_State"] <- "NAME"
            
            ObsAllSpatial_Bounds <- rbind(df2[ , -which(names(df2) %in% c("Tribe","Secondary_Tribe"))],
                                          Tribes_col[ , -which(names(Tribes_col) %in% c("Region_State","Secondary_Tribe"))],
                                          Tribes_col2[ , -which(names(Tribes_col2) %in% c("Region_State","Tribe"))])
        }
        index <- 1 + which(colnames(ObsAllSpatial_Bounds)=="Hardness" )
        
        ## Cap hardness values based on specific criteria
        obsCapped <- within(ObsAllSpatial_Bounds, Hardness[Hardness>400] <- 400) #Maximum hardness of 400 mg/L for most criteria in the region
        
        if (input$Categories==TRUE)  {
            GroupCategories <- colnames(ObsAllSpatial_Bounds) [(which(colnames(ObsAllSpatial_Bounds)=="Lon")+1):(which(colnames(ObsAllSpatial_Bounds)=="Hardness")-1)]
            ScreenCategories <- c("NAME",GroupCategories)
            UniqueObs <- unique(obsCapped[ScreenCategories])
            OutputCategories <- c("Designated_Use","ScreenType","NAME",GroupCategories,"Metal","Times_Exceeded","Number_Screened")
            
            output_screen <- data.frame(matrix(ncol = length(OutputCategories), nrow = rows), 
                                  stringsAsFactors=FALSE)
            names(output_screen) <- OutputCategories
            output_screen[,OutputCategories] <- lapply(output_screen[,OutputCategories],as.character)
            output_screen$Times_Exceeded <- as.numeric(output_screen$Times_Exceeded)
            output_screen$Number_Screened <- as.numeric(output_screen$Number_Screened)
            output_screen$Times_Exceeded[is.na(output_screen$Times_Exceeded)] <- 0
            output_screen$Number_Screened[is.na(output_screen$Number_Screened)] <- 0
            output_screen[is.na(output_screen)] <- ""
        } else {
            UniqueObs <- unique(obsCapped[c("NAME","Sample_Type")])
            output_screen <- data.frame(Designated_Use = character(rows), 
                                        ScreenType = character(rows), 
                                        NAME = character(rows), 
                                        Sample_Type = character(rows), 
                                        Metal = character(rows), 
                                        Times_Exceeded = numeric(rows), 
                                        Number_Screened = numeric(rows), 
                                        stringsAsFactors=FALSE)
        }
        
        ## This is the main function of the tool. For each sample the applicable screening criteria are identified and used to 
        ## determine the number of times a WQ criteria has been exceeded for a specific screen.
        for (i in 1:nrow(UniqueObs)) { #loops through each sample by unique combinations of region and conc type(row)
            print(UniqueObs[i,])
            
            if (input$Categories==TRUE)  { # Converts designated columns into Categories
                filtercolumns <- which((names(obsCapped) %in% names(UniqueObs[i,]))==TRUE)
                filt1 <- NULL
                filt2 <- NULL
                filtervar <- NULL
                for (l in 1:length(filtercolumns)){ # generates variable with string to pass to filter_
                    filt1[l] <- names(obsCapped[filtercolumns[l]])
                    filt2[l] <-UniqueObs[i,l]
                    filtervar[l] <-paste(filt1[l],"==","'",filt2[l],"'", sep="")
                }
                tempSamples <- filter_(obsCapped, filtervar) #subset observed data by unique combination
            } else {
                tempSamples <- filter(obsCapped, NAME==UniqueObs$NAME[i], Sample_Type==UniqueObs$Sample_Type[i]) #subset observed data by unique combination
            }
            for (j in index:ncol(obsCapped)){ #loops through each metal
                print(colnames(tempSamples[j]))
                
                if (UniqueObs$NAME[i]=="New Mexico" & 
                    UniqueObs$Sample_Type[i]=="Total" & 
                    colnames(tempSamples[j])=="Aluminum") { #New Mexico hardness limit for total Al = 220 mg/L
                    tempSamples <- within(tempSamples, Hardness[Hardness>220] <- 220)
                }
                hardness <- data.frame("Hardness" = tempSamples$Hardness, 
                                       "Conc" = tempSamples[j], 
                                       "ObsMetal" = colnames(tempSamples[j]),
                                       "Lat" = tempSamples$Lat,
                                       "Lon" = tempSamples$Lon,
                                       "Samp_No" = tempSamples$Samp_No,
                                       "Sp_Layer" = tempSamples$Sp_Layer,
                                       stringsAsFactors=FALSE) 
                
                screen <- filter(WQCritAll, 
                                 NAME==UniqueObs$NAME[i], 
                                 Sample_Type==UniqueObs$Sample_Type[i],
                                 variable==colnames(tempSamples[j])) #iteratively queries WQ criteria based on sample data (sample & metal)
                
                if (length(screen$value) > 0){
                    for (b in 1:length(screen$ScreenType)) { #loop through matching screens 
                        if (!is.na(screen$maSlope[b]==TRUE)) { #find screens that need to be calculated based on hardness
                            aquatic_screen <- data.frame(Sample_No = character(nrow(tempSamples)),
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
                                for (y in 1:nrow(hardness)) { #iterate through each sample 
                                    screen$value[b] <- as.numeric((exp((screen$maSlope[b]*log(hardness$Hardness[y]))+screen$mbIntercept[b])*screen$conversionFactor[b])/1000) #calculate criteria
                                    aquatic_screen[g,] <- c(hardness[y,6],
                                                            screen$Designated_Use[b],
                                                            hardness[y,7],
                                                            screen$ScreenType[b], 
                                                            hardness[y,4],
                                                            hardness[y,5],
                                                            screen$NAME[b],
                                                            screen$Sample_Type[b],
                                                            screen$variable[b],
                                                            as.numeric(screen$value[b]), 
                                                            as.numeric(hardness[y,2]), 
                                                            hardness[y,3]) #collect criteria and sample value (for screen eval)
                                    
                                    aquatic_screen[, c(10:11)] <- sapply(aquatic_screen[, c(10:11)], as.numeric)
                                    g=g+1
                                }
                            } else if (screen$alphaBeta[b] == 1) { #calculator function 2 
                                for (z in 1:nrow(hardness)) { #iterate through each sample
                                    screen$value[b] <- as.numeric((exp((screen$maSlope[b]*log(hardness$Hardness[z])+screen$mbIntercept[b]))*(screen$alpha[b]-(log(hardness$Hardness[z])*screen$beta[b])))/1000) #calculate criteria
                                    aquatic_screen[g,] <- c(hardness[z,6],
                                                            screen$Designated_Use[b],
                                                            hardness[z,7],
                                                            screen$ScreenType[b], 
                                                            hardness[z,4],
                                                            hardness[z,5],
                                                            screen$NAME[b],
                                                            screen$Sample_Type[b],
                                                            screen$variable[b],
                                                            as.numeric(screen$value[b]), 
                                                            as.numeric(hardness[z,2]), 
                                                            hardness[z,3]) #collect criteria and sample value (for screen eval)
                                    
                                    aquatic_screen[, c(10:11)] <- sapply(aquatic_screen[, c(10:11)], as.numeric)
                                    g=g+1
                                }
                            } else {
                                cat("Something went wrong with the hardness calculator.", "The error occured calculating the screening criteria for",screen$Sample_Type[b],
                                    screen$variable[b], "using the",screen$ScreenType[b], "screen for",screen$NAME[b])
                            }
                            aquatic_screen_cleaned <- filter(aquatic_screen, CalcValue >= 0, SampleValue >= 0, CritMetal != "") #remove empty rows in data.frame
                            n_screened <- nrow(aquatic_screen_cleaned) #count the number of samples that are screened
                            n_screened[is.null(n_screened)] <- -500
                            if (n_screened > 0) {
                                metal_vector_exceedances <- aquatic_screen_cleaned[which(aquatic_screen_cleaned$SampleValue > aquatic_screen_cleaned$CalcValue),] #filter criteria with exceedances
                                metal_exceedance_count <- nrow(metal_vector_exceedances) #count exceedances
                                m=m+1
                                
                                for (f in 1:nrow(aquatic_screen_cleaned)) {
                                    n=n+1
                                    Samplemarkerlayer[n,] <- aquatic_screen_cleaned[f,]
                                }
                                if (input$Categories==TRUE)  {
                                    nCategories <- (UniqueObs[,c(-1,-ncol(UniqueObs))])
                                    screenvars1 <- c(screen$Designated_Use[b], 
                                                     screen$ScreenType[b], 
                                                     screen$NAME[b])
                                    screenvars2 <- NULL
                                    for (x in 1:length(GroupCategories[-length(GroupCategories)])) {
                                        screenvars2[x] <- (nCategories[i,x])
                                    }
                                    screenvars3 <- c(screen$Sample_Type[b],
                                                     screen$variable[b],
                                                     metal_exceedance_count, 
                                                     n_screened)
                                    
                                    screenvarTot <- c(screenvars1,screenvars2,screenvars3)
                                    output_screen[m,] <- screenvarTot
                                } else {
                                    output_screen[m,] <- c(screen$Designated_Use[b], 
                                                           screen$ScreenType[b], 
                                                           screen$NAME[b], 
                                                           screen$Sample_Type[b],
                                                           screen$variable[b],
                                                           metal_exceedance_count, 
                                                           n_screened)
                                }
                            }
                            
                        } else {
                            metal_df <- cbind(tempSamples[1:(index-1)] ,tempSamples[screen$variable[b]])
                            
                            if (!all(is.na(tempSamples[screen$variable[b]]))) { #distinguishes between a non-detect sample and no sample
                                metal_vector_nonas <- metal_df[!is.na(metal_df[screen$variable[b]]),]#remove NAs
                                num_metal_samples <- nrow(metal_vector_nonas[screen$variable[b]]) #count the number of samples that are screened
                                num_metal_samples[is.null(num_metal_samples)] <- -500
                                if (num_metal_samples > 0) {
                                    metal_vector_exceedances <- metal_vector_nonas[which(metal_vector_nonas[screen$variable[b]]>screen$value[b]),] #filter criteria with exceedances
                                    metal_exceedance_count <- nrow(metal_vector_exceedances) #count exceedances
                                    m=m+1
                                    
                                    singlerowout <- data.frame(Designated_Use =screen$Designated_Use[b], 
                                                               ScreenType = screen$ScreenType[b], 
                                                               CritMetal = screen$variable[b],
                                                               CalcValue = screen$value[b],
                                                               ObsMetal = screen$variable[b],
                                                               stringsAsFactors=FALSE)
                                    
                                    reprowout <- singlerowout[rep(seq_len(nrow(singlerowout)), each=nrow(metal_vector_nonas)),]
                                    
                                    noncalc_criteria <- data.frame(Sample_No = metal_vector_nonas$Samp_No,
                                                                   Designated_Use = reprowout$Designated_Use, 
                                                                   Sp_Layer = metal_vector_nonas$Sp_Layer,
                                                                   ScreenType = reprowout$ScreenType, 
                                                                   Lat = metal_vector_nonas$Lat,
                                                                   Lon = metal_vector_nonas$Lon,
                                                                   NAME = metal_vector_nonas$NAME, 
                                                                   Sample_Type = metal_vector_nonas$Sample_Type,
                                                                   CritMetal = reprowout$CritMetal,
                                                                   CalcValue = reprowout$CalcValue,
                                                                   SampleValue = metal_vector_nonas[[screen$variable[b]]],
                                                                   ObsMetal = reprowout$CritMetal,
                                                                   stringsAsFactors=FALSE)
                                    
                                    for (t in 1:nrow(noncalc_criteria)) {
                                        n=n+1
                                        Samplemarkerlayer[n,] <- noncalc_criteria[t,]            
                                    }
                                    if (input$Categories==TRUE)  {
                                        nCategories <- (UniqueObs[,c(-1,-ncol(UniqueObs))])
                                        screenvars1 <- c(screen$Designated_Use[b], 
                                                         screen$ScreenType[b], 
                                                         screen$NAME[b])
                                        screenvars2 <- NULL
                                        for (x in 1:length(GroupCategories[-length(GroupCategories)])) {
                                            screenvars2[x] <- (nCategories[i,x])
                                        }
                                        screenvars3 <- c(screen$Sample_Type[b],
                                                         screen$variable[b],
                                                         metal_exceedance_count, 
                                                         n_screened)
                                        
                                        screenvarTot <- c(screenvars1,screenvars2,screenvars3)
                                        output_screen[m,] <- screenvarTot
                                    } else {
                                    output_screen[m,] <- c(screen$Designated_Use[b], 
                                                           screen$ScreenType[b], 
                                                           screen$NAME[b], 
                                                           screen$Sample_Type[b],
                                                           screen$variable[b],
                                                           metal_exceedance_count, 
                                                           num_metal_samples) 
                                    }
                                }
                            }
                        }
                    }
                } else {
                    cat(UniqueObs$Sample_Type[i], 
                        colnames(tempSamples[j]), 
                        UniqueObs$NAME[i], 
                        file="echoFile.txt", append=TRUE)
                }
                
            }
        }
        output_screen <- filter(output_screen, ScreenType!="")
        output_screen$Times_Exceeded <- as.numeric(output_screen$Times_Exceeded)
        output_screen_Exceeded <- filter(output_screen, Times_Exceeded > 0)
        write.csv(WQCritAll,file="WQCritAll.csv")
        
        if (exists("Samplemarkerlayer")){ 
            #write.csv(samplemarkers,file="SamplelatlonOutput.csv",row.names = FALSE)
            samplemarkers_screen <- filter(Samplemarkerlayer, ScreenType!="") %>%
                mutate(Difference = SampleValue/CalcValue , Type = ifelse(Difference < 1,"NotExceeded","Exceeded"))
            samplemarkers_screen$Lat <- as.numeric(samplemarkers_screen$Lat)
            samplemarkers_screen$Lon <- as.numeric(samplemarkers_screen$Lon)
            write.csv(samplemarkers_screen,file="Samplelatlondiferences.csv",row.names = FALSE)
        }
        
        output$Results = renderDataTable({
            if (input$checked==FALSE)  {output_screen
            } else {
                if (input$checked==TRUE) {output_screen_Exceeded}
            }
        })
        
        output$Pivot1 = renderRpivotTable({
            rpivotTable(data=output_screen, rows = c("Designated_Use","Time_Period"),cols = c("Metal","River","NAME"), rendererName = "Bar Chart",aggregatorName = "Sum over Sum", vals = c("Times_Exceeded","Number_Screened"))
        })
        output$screenprogress <- renderPrint({
            message("Screen Complete")
            cat("Screen Complete")
            #message(samplemarkers)
        })
    
    
    ################## End  of what click does ############
    #Filter data
    datFilt <- reactive({ 
        #if (exists("samplemarkers_screen")){ 
        samplemarkers_screen[samplemarkers_screen$Sp_Layer == input$Authority & 
                                                   samplemarkers_screen$CritMetal == input$Contaminant & 
                                                   samplemarkers_screen$Sample_Type == input$Sampletype & 
                                                   samplemarkers_screen$ScreenType == input$Criteria,]
        #}
    })
    
    output$Samplenrows <- reactive({
        nrow(datFilt())
        print(nrow(datFilt()))
    })
   
    observe({
        outputOptions(output, "Samplenrows", suspendWhenHidden = FALSE)  
    })
    
    #samplemarkers_screen$NAME == input$Authority & & samplemarkers_screen$Sample_Type == input$Sampletype
    #Tot_Al_markers <- filter(samplemarkers_screen, Designated_Use == "Aquatic Acute", Sample_Type == "Total", ObsMetal == "Aluminum")
    samplepal <- colorFactor(c("red","navy"), domain = c("NotExceeded","Exceeded"))
    #datFilt <- reactive(mydat[flag%in%input$InFlags])

    observeEvent(input$Clickmap, {
        QueryCrieria<-reactive({
            # Get a subset of the criteria data based on drop down box selection
            dataSet <- WQCritAll[WQCritAll$Spatial_Type == input$Authority & 
                                     WQCritAll$variable == input$Contaminant & 
                                     WQCritAll$Sample_Type == input$Sampletype & 
                                     WQCritAll$ScreenType == input$Criteria,] #c("Spatial_Bound","value")
            # Copy our GIS data
            joinedDataset<-Authority_Layer()
            
            # Join the two datasets together
            joinedDataset@data <- merge(dataSet,joinedDataset@data)
            joinedDataset
        })
        
        #observe({
        if(is.null(datFilt())) {
            print("Nothing selected")
            #leafletProxy("map") %>% clearMarkers()
        }
        else{
            if (input$ImportedSamples==FALSE)  {
                leafletProxy("map") %>% clearMarkers()
            } else {
                if (input$ImportedSamples==TRUE) {
                    leafletProxy("map",data=datFilt()) %>%
                        clearMarkers() %>%
                        addCircleMarkers(~Lon, ~Lat,
                                         #layerId = "ImportedSamples",
                                         radius = ~ifelse(Type == "NotExceeded", 3, ifelse(Difference >5,5,3+Difference)), #abs(Difference)
                                         color = ~samplepal(Type),
                                         popup = ~as.character(Sample_No),
                                         stroke = FALSE, 
                                         fillOpacity = 0.5,
                                         #clusterOptions=markerClusterOptions(),
                                         group = "Imported Samples") %>%
                        fitBounds(~min(Lon), ~min(Lat), ~max(Lon), ~max(Lat))
                    #addLayersControl(
                    #overlayGroups = "Imported Samples",
                    #options = layersControlOptions(collapsed = FALSE))
                } }
        }
        
    })
    #})
    
    #observe({
    # if(is.null(QueryCrieria())) {
    #   print("No Citeria Selected")
    #   #leafletProxy("map") %>% clearMarkers()
    # }
    # else{
    #   sp_selection <- QueryCrieria()
    #   
    #   sp_dataframe <- sp_selection@data
    #   #pal <- colorQuantile("YlGn", joinedDataset$value, n = 5) 
    #   binpal <- colorBin("Blues", sp_selection$value, 6, pretty = FALSE)
    #   #legendvalues <- sp_selection$value
    #   Criteria_popup <- paste0("<strong>State: </strong>", 
    #                            sp_selection$NAME, 
    #                            "<br><strong>Criteria: </strong>",
    #                            sp_selection$ScreenType,
    #                            "<br><strong>Contaminant: </strong>",
    #                            sp_selection$Sample_Type," ",
    #                            sp_selection$variable,
    #                            "<br><strong>Concentration Limit: </strong>", 
    #                            formatC(sp_selection$value, big.mark=','))
    #   
    #   
    #   if(is.null(QueryCrieria())) {
    #     print("Nothing selected")
    #     leafletProxy("map") #%>% clearMarkers()
    #   }
    #   else{
    #     leafletProxy("map",data = QueryCrieria()) %>%
    #       clearShapes() %>%
    #       addPolygons(color = "#A9A9A9",
    #                   fillColor = ~binpal(sp_selection$value), 
    #                   stroke = TRUE, smoothFactor = 0.2,
    #                   opacity = 1.0, fillOpacity = 0.5, weight = 1,
    #                   popup = Criteria_popup) %>%
    #       setView(lng = -98.35, lat = 39.5,  zoom = 4) 
    #     #addLegend("bottomleft",pal = binpal, values = ~sp_dataframe$values, opacity = 1)
    #   }
    # }
    #})
})
}
shinyApp(ui= ui, server = server)