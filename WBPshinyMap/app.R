#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(sf)
library(tidyverse)
library(leaflet)
library(viridis) # creates colours for plots
library(fontawesome)
library(htmltools)
library(leaflegend) # legend for leaflet AwesomeIcons

# Load data
# Collection trees
alltrees <- fread("C:/Users/farne/OneDrive/Documents/Borealis_Ecological_Services/WBPrestoration/Inputs/Seed Collection - All years.csv") # check this is the most recent version if it has been a while since last used
alltrees[Screening == "", Screening := "N"]

# Planted areas - spatial
plant.sf <- st_read(dsn = "C:/Users/farne/OneDrive/Documents/Borealis_Ecological_Services/WBPrestoration/Inputs/AllPlantingLocations.gpkg")
plant.sf <- st_zm(plant.sf, drop=TRUE) # remove the z dimension or it won't plot

# Planted areas - data
plant <- fread("C:/Users/farne/OneDrive/Documents/Borealis_Ecological_Services/WBPrestoration/Inputs/Planting_AllYears.csv")

# ARU sites
ARU <- fread("C:/Users/farne/OneDrive/Documents/Borealis_Ecological_Services/WBPrestoration/Inputs/WBP-ARU_data_AllYears.csv")
ARU.sf <- st_as_sf(ARU, coords = c("Longitude", "Latitude"),
                   crs = "+proj=longlat")

# Camera sites
cam <- fread("C:/Users/farne/OneDrive/Documents/Borealis_Ecological_Services/WBPrestoration/Inputs/CamSetupData2023.csv")
cam.sf <- st_as_sf(cam, coords = c("Longitude", "Latitude"),
                   crs = "+proj=longlat")

# WBP range
range.sf <- st_read(dsn = "C:/Users/farne/OneDrive/Documents/Borealis_Ecological_Services/WBPrestoration/Inputs/WBP_Range_clason.gpkg")
range.sf <- st_transform(range.sf, crs = "+proj=longlat")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Whitebark Pine Restoration Synthesis"),

        # Show a map of all WBP restoration activties
        mainPanel(
          leafletOutput("map", 
                        width = "800px",
                        height = "600px")
        )
    )

# Define server logic required to draw the map
server <- function(input, output) {

  output$map <- renderLeaflet({
    
    
    #-- Map of collection trees, restoration areas, ARU sites
    # collection trees: column for icon/colour
    alltrees[, ScreenResults := ifelse(Elite_tree == "Y", "Elite",
                                       ifelse(Screening == "Y", "Screened", "Not screened"))]
    #remove rows with missing lat long - they are the bulk lots
    temp <- alltrees[!is.na(Longitude) & Longitude != ""]
    # convert dt to sf
    alltrees.sf <- st_as_sf(temp, coords = c("Longitude", "Latitude"), 
                            crs = "+proj=longlat")
    alltrees.sf$YearColl <- as.factor(alltrees.sf$YearColl)
    
    # planted: 
    plant.sf$Area <- as.numeric(plant.sf$Area)
    plant.sf$Area_ha <- round(plant.sf$Area*0.0001, digits = 1)
    
    #colours based on year planted
    plant.sf$Year <- as.factor(plant.sf$YrPlntd)
    colour <- colorFactor(
      palette = c("purple", "blue", "red", "pink", "orange", "green"),
      domain = c("2011", "2012", "2014", "2017", "2021", "2023")
    )
    
    # create colours and icons based on screened and elite trees
    # list of icons
    treeIcons <- awesomeIconList(
      "Elite" = makeAwesomeIcon(
        icon = "star",
        library = "fa",
        markerColor = "purple",
        iconColor = "black"
      ),
      "Screened" = makeAwesomeIcon(
        icon = NA,
        library = "fa",
        markerColor = "purple"
      ),
      "Not screened" = makeAwesomeIcon(
        icon = NA,
        library = "fa",
        markerColor = "blue"
      )
    )
    
    ARUicons <- makeAwesomeIcon(
      text = fa("crow"), # for some reason have to write it like this for crow to show up
      library = "fa",
      markerColor = "orange"
    )
    
    cameraIcons <- makeAwesomeIcon(
      icon = "camera",
      library = "fa",
      markerColor = "orange"
    )
    
    # Create habitat use icons for legend
    habLegendIcons <- awesomeIconList(
      "ARU sites" = makeAwesomeIcon(
        text = fa("crow"),
        icon = NA,
        library = "fa",
        markerColor = "orange",
        iconColor = "black"
      ),
      "Camera sites" = makeAwesomeIcon(
        icon = "camera",
        library = "fa",
        markerColor = "orange",
        iconColor = "black"
      )
    )
    
    
    # Map
    p <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>% # ESRI satelite imagery
      # 2011 collection trees
      addAwesomeMarkers(data=alltrees.sf[alltrees.sf$YearColl == 2011,],
                        icon = ~treeIcons[ScreenResults],
                        popup = ~paste(Tree_ID,
                                       "<br>Year collected:", YearColl,
                                       "<br>Location:", Location),
                        group = "2011 Seed Collection") %>%
      # 2013 collection trees
      addAwesomeMarkers(data=alltrees.sf[alltrees.sf$YearColl == 2013,],
                        icon = ~treeIcons[ScreenResults],
                        popup = ~paste(Tree_ID,
                                       "<br>Year collected:", YearColl,
                                       "<br>Location:", Location),
                        group = "2013 Seed Collection") %>%
      # 2018 collection trees
      addAwesomeMarkers(data=alltrees.sf[alltrees.sf$YearColl == 2018,],
                        icon = ~treeIcons[ScreenResults],
                        popup = ~paste(Tree_ID,
                                       "<br>Year collected:", YearColl,
                                       "<br>Location:", Location),
                        group = "2018 Seed Collection") %>%
      # 2022 collection trees
      addAwesomeMarkers(data = alltrees.sf[alltrees.sf$YearColl == "2022",],
                        icon = ~treeIcons[ScreenResults],
                        popup = ~paste(Tree_ID,
                                       "<br>Year collected:", YearColl,
                                       "<br>Location:", Location),
                        group = "2022 Seed Collection") %>%
      # ARU sites
      addAwesomeMarkers(data = ARU.sf,
                        icon = ARUicons,
                        popup = ~paste("<br>Location:", Location,
                                       "<br>Deployment year:", Year),
                        group = "ARU sites") %>%
      # Camera sites
      addAwesomeMarkers(data = cam.sf,
                        icon = cameraIcons,
                        popup = ~paste("<br>Location:", Location,
                                       "<br>Deployment year:", Year),
                        group = "Camera sites") %>%
      # # add layers on/off control
      addLayersControl(overlayGroups = c("2011 Seed Collection",
                                         "2013 Seed Collection",
                                         "2018 Seed Collection",
                                         "2022 Seed Collection",
                                         "Planting Locations",
                                         "ARU sites",
                                         "Camera sites",
                                         "WBP range"),
                       position = "topright") %>%
      # Collection tree legend
      addLegendAwesomeIcon(
        iconSet = treeIcons,
        title = "Collection Trees",
        position = "topright",
        orientation = "vertical") %>%
      # Habitat use legend
      addLegendAwesomeIcon(
        iconSet = habLegendIcons,
        title = "Habitat Use",
        position = "topright",
        orientation = "vertical") %>%
      #WBP range
      addPolygons(data = range.sf,
                  color = "green",
                  fillOpacity = 0.25,
                  group = "WBP range") %>%
      # Restoration plant areas
      addPolygons(data = plant.sf,
                  color = ~colour(Year),
                  popup = ~paste(Name, 
                                 "<br>", Location,
                                 "<br>Year planted:", Year,
                                 "<br> Area (ha):", Area_ha,
                                 "<br> # Seedings:", NumSeedlings),
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  group = "Planting Locations") %>%
      # Planting colour legend
      addLegend("topright", pal = colour, values = plant.sf$Year,
                title = "Year Planted",
                opacity = 1)

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
