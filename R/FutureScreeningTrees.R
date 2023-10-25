# Rust screening tree locations

# this script plots the location of all trees that seeds have been collected from, 
# buffers the previously accepted trees by 50 m and populates a dataset of 
# trees that can be submitted for future screenings

# load libraries
library(data.table)
library(leaflet)
library(viridis) # creates colours for plots
library(sf)
library(tidyverse)

# load data
alltrees <- fread("./Inputs/Seed Collection - All years.csv")
alltrees[Screening == "", Screening := "N"]

# Check out data
# create colours based on whether screened or not
alltrees[, Screening := as.factor(Screening)]
colour <- colorFactor(
  palette = c("purple", "blue", "red"),
  domain = c("N", "No poor germ", "Y")
)

p <- leaflet(alltrees) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>% # ESRI satelite imagery
  addCircleMarkers(lng=alltrees$Longitude, lat=alltrees$Latitude,
                   color= ~colour(Screening),
                   popup=paste(alltrees$Tree_ID),
                   stroke = FALSE, fillOpacity = 0.5
  )
p

# Buffer trees that have been screened by 50 m (trees that did not germinate can be resubmitted)

# filter trees that have been screened
screened <- alltrees.sf[alltrees.sf$Screening == "Y",]
# create a 50 m buffer
screened.buffer <- st_buffer(screened, dist = 50)
#remove rows with missing lat long - they are the bulk lots
temp <- alltrees[!is.na(Longitude) & Longitude != ""]
# convert dt to sf
alltrees.sf <- st_as_sf(temp, coords = c("Longitude", "Latitude"), 
                        crs = "+proj=longlat")

# check out the buffers
pp <- p %>%
  addPolygons(
    data = screened.buffer)
pp

# keep only the trees outside the buffers
intersect <- st_intersects(alltrees.sf, screened.buffer)
intersect.log <- lengths(intersect) == 0 # 1 = TRUE (within intersect) 0 = FALSE (outside intersect)
FutureScreen <- filter(alltrees.sf, intersect.log)

# visual check that there are no points within the buffers
fp <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>% # ESRI satelite imagery
  addCircleMarkers(data=FutureScreen,
                   popup=paste(FutureScreen$Tree_ID),
                   color = "purple",
                   stroke = FALSE, fillOpacity = 1) %>%
  addPolygons(
    data = screened.buffer)
fp

# Only keep relevant columns for seed submission to Kalamalka
FutureScreen.dt <- as.data.table(FutureScreen)
FutureScreen.dt <- cbind(FutureScreen.dt, st_coordinates(FutureScreen))
columnstokeep <- c("Tree_ID", "Unique_ID", "Location", "LandOwnership", "X", "Y", 
                   "ABC", "IABC", "ASC", "IASC", "Infected", 
                  "TreesKilled_byRustMPB")
FutureScreen.dt <- FutureScreen.dt[, .SD, .SDcols=columnstokeep]

# rename columns to match Kalamalka
FutureScreen.dt[, Active := ABC + ASC][, Inactive := IABC + IASC]

oldcol <- c("X", "Y", "Active", "Inactive", "Infected", "TreesKilled_byRustMPB")
newcol <- c("Longitude", "Latitude", "No. Cankers (active)", "No. Cankers (inactive",
            "% Live Trees infected", "% Trees Killed by rust and mpb")
setnames(FutureScreen.dt, old=oldcol, new=newcol)
FutureScreen.dt[, c("ABC", "IABC", "ASC", "IASC") := NULL]

# add a few extra columns
FutureScreen.dt[, Contact := "Alana Clason"][, `BC/AB` := "BC"]
str(FutureScreen.dt)


# export the dataset
write.csv(FutureScreen.dt, "./Outputs/FutureScreeningTrees.csv")
