# Libraries
library(data.table)
library(sf)
library(tidyverse)
library(leaflet)
library(viridis) # creates colours for plots
library(fontawesome)
library(shiny)
library(htmltools)
library(leaflegend) # legend for leaflet AwesomeIcons

# Load data
# Collection trees
alltrees <- fread("./Inputs/Seed Collection - All years.csv") # check this is the most recent version if it has been a while since last used
alltrees[Screening == "", Screening := "N"]

# Planted areas - spatial
plant <- st_read(dsn = "./Inputs/AllPlantingLocations.gpkg")
plant <- st_zm(plant, drop=TRUE) # remove the z dimension or it won't plot

# Planted areas - data
plantDat <- fread("./Inputs/Planting_AllYears.csv")

# ARU sites
ARU <- fread("./Inputs/WBP-ARU_data_AllYears.csv")
ARU.sf <- st_as_sf(ARU, coords = c("Longitude", "Latitude"),
                   crs = "+proj=longlat")

# Camera sites
cam <- fread("./Inputs/CamSetupData2023.csv")
cam.sf <- st_as_sf(cam, coords = c("Longitude", "Latitude"),
                   crs = "+proj=longlat")

# WBP range
range.sf <- st_read(dsn = "./Inputs/WBP_Range_clason.gpkg")
range.sf <- st_transform(range.sf, crs = "+proj=longlat")

#-- Map of collection trees, restoration areas, ARU sites
plant$Area <- as.numeric(plant$Area)
plant$Area_ha <- round(plant$Area*0.0001, digits = 1)

# collection trees: column for icon/colour
alltrees[, ScreenResults := ifelse(Elite_tree == "Y", "Elite", 
                                   ifelse(Screening == "Y", "Screened", "Not screened"))]
#remove rows with missing lat long - they are the bulk lots
temp <- alltrees[!is.na(Longitude) & Longitude != ""]
# convert dt to sf
alltrees.sf <- st_as_sf(temp, coords = c("Longitude", "Latitude"), 
                        crs = "+proj=longlat")
alltrees.sf$YearColl <- as.factor(alltrees.sf$YearColl)

# planted: colours based on year planted
plant$Year <- as.factor(plant$YrPlntd)
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
    markerColor = "purple",
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


#-------------------------------------------------
# Define UI ----
ui <- fluidPage(
titlePanel("Whitebark Pine Restoration Synthesis"),

sidebarLayout(
  sidebarPanel("sidebar panel"),
  mainPanel(
    p("This map presents all of the whiebark pine restoration activities that the BVRC 
      has carried out. This includes cone collection, rust screening, and planting seedlings."),
    p("In future years we would like to better understand the relationship between wildlife 
      and whitebark pine. We have set up a pilot project using ARUs and camera traps to observe
      what animals are using these pine ecosystems."),
    leafletOutput("map")
  )
  )

)

# Define server logic ----
server <- function(input, output) {

  output$map <- renderLeaflet({
    leaflet() %>%
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
      addAwesomeMarkers(data=alltrees.sf[alltrees.sf$YearColl == 2022,],
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
      # add layers on/off control
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
      addPolygons(data = plant,
                  color = ~colour(Year),
                  popup = ~paste(Name,
                                 "<br>Year planted:", Year,
                                 "<br> Area (ha):", Area_ha),
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  group = "Planting Locations") %>%
      # Planting colour legend
      addLegend("topright", pal = colour, values = plant$Year,
                title = "Year Planted",
                opacity = 1)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)




#--------------------------------------------------
# COLLECTION TREES

# Number of collected trees by year
filtcolltrees <- unique(alltrees$Tree_ID)
# remove the seedlots and bulked seeds from the unique collection trees
filtalltrees <- alltrees[Tree_ID %in% filtcolltrees]
colltrees <- filtalltrees[, .(NumCollTrees = uniqueN(Tree_ID)), by=YearColl]
filtcolltrees <- colltrees[!grepl("seedlot|bulk", colltrees, ignore.case=TRUE)]


# kg of seed collected
alltrees[Total_Seeds_g == "", Total_Seeds_g:= NA]
collSummary <- alltrees[!is.na(Total_Seeds_g), .(Seeds_g = sum(Total_Seeds_g)), by=YearColl]
collSummary[, Seeds_kg := Seeds_g*0.001] # convert g to kg

collSummary <- merge(collSummary, colltrees)
collSummary[, Seeds_g:=NULL]

# Number of trees identified as elite trees
elite <- alltrees[Elite_tree == "Y", .(Elite = uniqueN(Tree_ID))]

# Number of trees in the screening program
screening <- alltrees[Screening == "Y", .(Screening = uniqueN(Tree_ID))]

# Number of plus trees *not sure which trees are not plus trees*


#----------------------------------------------------
# RESTORATION AREA

# Area planted
plantDat[, Area_ha := Area_m2*0.0001]


# Number of trees planted *need info from Sybille*


#------------------------------------------------------
# HABITAT USE (area of future work)
# Clark's nutcracker


# Camera traps

#---------------------------------------------------------------
#-- Map of collection trees, restoration areas, ARU sites
plant$Area <- as.numeric(plant$Area)
plant$Area_ha <- round(plant$Area*0.0001, digits = 1)

# collection trees: column for icon/colour
alltrees[, ScreenResults := ifelse(Elite_tree == "Y", "Elite", 
                                   ifelse(Screening == "Y", "Screened", "Not screened"))]
#remove rows with missing lat long - they are the bulk lots
temp <- alltrees[!is.na(Longitude) & Longitude != ""]
# convert dt to sf
alltrees.sf <- st_as_sf(temp, coords = c("Longitude", "Latitude"), 
                        crs = "+proj=longlat")
alltrees.sf$YearColl <- as.factor(alltrees.sf$YearColl)

# planted: colours based on year planted
plant$Year <- as.factor(plant$YrPlntd)
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
    markerColor = "purple",
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
  addAwesomeMarkers(data=alltrees.sf[alltrees.sf$YearColl == 2022,],
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
  # add layers on/off control
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
  addPolygons(data = plant,
              color = ~colour(Year),
              popup = ~paste(Name,
                             "<br>Year planted:", Year,
                             "<br> Area (ha):", Area_ha),
              fillOpacity = 1,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              group = "Planting Locations") %>%
  # Planting colour legend
  addLegend("topright", pal = colour, values = plant$Year,
            title = "Year Planted",
            opacity = 1)

p

