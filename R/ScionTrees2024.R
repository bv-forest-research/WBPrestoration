# Scion collection 2024
# This script just isolates the collection trees for this year's scion collection

# Load libraries
library(data.table)
library(sf)


# Load data
trees <- fread("./Inputs/Seed Collection - All years.csv")

# trees selected for scion collection
scion <- trees[Unique_ID %in% c("1035", "1089", "1080", "1045", "1030", "1037",
                             "1078", "1085", "1028", "1026", "1072"), ]
# keep unique 
scion <- unique(scion, by = "Unique_ID")

# convert to spatial
scion.sf <- st_as_sf(scion, coords = c("Longitude", "Latitude"), 
                     crs = 4326)

# export geopackage and kml
st_write(scion.sf, "./Outputs/Scion2024.gpkg", driver = "GPKG")
st_write(scion.sf, "./Outputs/Scion2024.kml", driver = "KML")
