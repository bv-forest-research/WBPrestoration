# HCTF Funding proposal Map
# Nov 1, 2023

# libraries
packageList <- c(
  "data.table", # data manipulation
  "sf", # spatial data
  "dplyr", # data manipulation
  "leaflet", # interactive maps
  "viridis", # creates colours for leaflet::colorFactor
  "fontawesome", # icons for leaflet maps
  "leaflegend", # legend for awesome icons
  "htmltools" # html code for leaflet maps
)
# Check you have them in your library
new.packages <- packageList[!(packageList %in% installed.packages()[,"Package"])]
# load them
if(length(new.packages)) install.packages(new.packages)
lapply(packageList, require, character.only = TRUE)
rm(packageList, new.packages)


# Data
# proposed camera sites
cam <- st_read(dsn = "./Inputs/ProposedCameraESSFmk2024.gpkg")
# proposed collection sites
collection <- st_read(dsn = "./Inputs/ProposedCollection2024.gpkg")
# WBP range
range.sf <- st_read(dsn = "./Inputs/WBP_Range_clason.gpkg")
range.sf <- st_transform(range.sf, crs = "+proj=longlat")


#------------------------------------
# make collection icon
collectionIcon <- makeAwesomeIcon(
  icon = NA,
  library = "fa",
  markerColor = "purple"
)
# create camera icons
cameraIcons <- makeAwesomeIcon(
  icon = "camera",
  library = "fa",
  markerColor = "orange"
)
# Create icons for legend
LegendIcons <- awesomeIconList(
  "Seed collection sites" = makeAwesomeIcon(
    icon = NA,
    library = "fa",
    markerColor = "purple",
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
  # Proposed collection sites 2024
  addAwesomeMarkers(data = collection,
                    icon = collectionIcon,
                    popup = ~paste("<br>Location:", Site),
                    group = "Seed collection sites") %>%
  # Camera sites
  addAwesomeMarkers(data = cam,
                    icon = cameraIcons,
                    popup = ~paste("<br>Location:", Site),
                    group = "Camera sites") %>%
  # # add layers on/off control
  addLayersControl(overlayGroups = c("Seed collection sites",
                                     "Camera sites",
                                     "WBP range"),
                   position = "topright") %>%
  # Legend
  addLegendAwesomeIcon(
    iconSet = LegendIcons,
    title = "2024 Sites",
    position = "topright",
    orientation = "vertical") %>%
  #WBP range
  addPolygons(data = range.sf,
              color = "green",
              fillOpacity = 0.25,
              group = "WBP range")

p
