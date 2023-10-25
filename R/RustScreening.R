
old_trees <- fread("./Inputs/Data/Whitebark_TSC_seeds_AC2022_treeSummary.csv")
#old_trees[,NewLong := as.numeric(ifelse(Long < 0, Long, as.numeric(paste0("-",Long))))]
old_trees_sf <- st_make_valid(st_as_sf(old_trees, coords = c("Lat", "NewLong"), crs = 4326))
plot(old_trees_sf$geometry)

# creating a winners layer and losers layer
all_trees <- read_sf("./Inputs/SpatialData/Restoration/AllCollectionTrees.shp")
all_trees_dt <- as.data.table(all_trees)
all_trees_dt <- all_trees_dt[,.(Parent.Tre,Coll..Tree,Origin,Coll..Yr.,
                                Ht..m., DBH..cm., geometry)]
setnames(all_trees_dt, old = c("Parent.Tre","Coll..Tree","Origin", "Coll..Yr.", "Ht..m.", "DBH..cm."),
         new = c("TreeTag", "FieldID","Site", "CollYr", "Ht", "DBH"))
all_trees_dt[,FieldID := gsub(" ", "",FieldID)]

#high = high rust resistance (do collect). low = low rust resistance (don't collect)
high_trees <- all_trees_dt[TreeTag == "1080"|TreeTag == "1035"|TreeTag == "1037"|
                             TreeTag == "1089"]
med_trees <- all_trees_dt[TreeTag == "1076"|TreeTag == "1030"|TreeTag == "1085"]

low_trees <- all_trees_dt[TreeTag == "1026"|TreeTag == "1028"|TreeTag == "1029"|
                            TreeTag == "1068"|TreeTag == "1072"|TreeTag == "1078"|
                            TreeTag == "1081"|TreeTag == "1045"]
write_sf(high_trees, "./Inputs/SpatialData/Restoration/high_rr.shp")
write_sf(high_trees, "./Inputs/SpatialData/Restoration/high_rr.kml")

write_sf(med_trees, "./Inputs/SpatialData/Restoration/med_rr.shp")
write_sf(med_trees, "./Inputs/SpatialData/Restoration/med_rr.kml")

write_sf(low_trees, "./Inputs/SpatialData/Restoration/low_rr.shp")
write_sf(low_trees, "./Inputs/SpatialData/Restoration/low_rr.kml")

#updating Sybille's raw tree screening data
raw_trees <- fread("./Inputs/Data/Raw_tree_screening.csv")

tr_screen <- melt(raw_trees, 
                  measure.vars = c("Cartwright", "Murray", "Maholovich-Idaho", "Sniezko-Dorena"),
                  variable.name = "Screen_facility", value.name = "Actual_inoc_year")
tr_screen <- tr_screen[,.(Family,UniqueNo,Collected,Inoc_year,Screen_facility,Actual_inoc_year,
                          MurrayIndex, Index_year = 2019 , SH_2019_Rank ,Notes)]
#write out to input new data
write.csv(tr_screen, "./Inputs/Data/Ed_tree_screening.csv", row.names = FALSE)

#after inputting the new data, read it back in and summarize the ranking

tr_scrn <- fread("./Inputs/Data/Ed_tree_screening_2022entered.csv")
tr_scrn_ind <- tr_scrn[!is.na(MurrayIndex) & Screen_facility == "Murray"]

tr_scrn_ind[,rank := ifelse(MurrayIndex < 1, "VH",
                            ifelse(MurrayIndex < 2, "H",
                                   ifelse(MurrayIndex < 3, "M",
                                          "L")))]
tr_scrn_ind <- tr_scrn_ind[,-c("Notes")]
tr_scrn_ind[,.(Family, UniqueNo,MurrayIndex,Index_year,SH_2019_Rank,rank)]

length(tr_scrn_ind[Index_year ==2022,.(Family, UniqueNo,MurrayIndex,Index_year,SH_2019_Rank,rank)]$rank)
unique(tr_scrn_ind[Index_year ==2022,.(Family, UniqueNo,MurrayIndex,Index_year,SH_2019_Rank,rank)]$UniqueNo)

length(tr_scrn_ind[Index_year ==2019,.(Family, UniqueNo,MurrayIndex,Index_year,SH_2019_Rank,rank)]$rank)
unique(tr_scrn_ind[Index_year ==2019,.(Family, UniqueNo,MurrayIndex,Index_year,SH_2019_Rank,rank)]$UniqueNo)

#high = high rust resistance (do collect). low = low rust resistance (don't collect)
tr_scrn_ind[Family=="DU04", Family:= "DU4"]
tr_scrn_ind[Family=="DU4"]

high_tr_1 <- tr_scrn_ind[!is.na(UniqueNo) & Index_year ==2022 & rank=="H"|
                           !is.na(UniqueNo) & Index_year ==2022 & rank=="VH",
                         .(Family,UniqueNo,rank)]$UniqueNo
high_tr_2 <- tr_scrn_ind[is.na(UniqueNo) & Index_year ==2022 & rank=="H"|
                           is.na(UniqueNo) & Index_year ==2022 & rank=="VH",
                         .(Family,UniqueNo,rank)]$Family

med_tr <- tr_scrn_ind[!is.na(UniqueNo) & Index_year ==2022 & rank=="M",
                      .(UniqueNo,rank)]$UniqueNo
med_tr_2 <- tr_scrn_ind[is.na(UniqueNo) & Index_year ==2022 & rank=="M",
                        .(Family,UniqueNo,rank)]$Family

low_tr <- tr_scrn_ind[!is.na(UniqueNo) & Index_year ==2022 & rank=="L",
                      .(UniqueNo,rank)]$UniqueNo
low_tr_2 <- tr_scrn_ind[is.na(UniqueNo) & Index_year ==2022 & rank=="L",
                        .(Family,UniqueNo,rank)]$Family

# select the gps points associated with high, med, low rust resistance
high_trees <- all_trees_dt[TreeTag %in% high_tr_1 | FieldID %in% high_tr_2]
med_trees <- all_trees_dt[TreeTag %in% med_tr | FieldID %in% med_tr_2]
low_trees <- all_trees_dt[TreeTag %in% low_tr | FieldID %in% low_tr_2]

write_sf(med_trees, "./Inputs/SpatialData/Restoration/med_rr.shp")
write_sf(med_trees, "./Inputs/SpatialData/Restoration/med_rr.kml")

write_sf(low_trees, "./Inputs/SpatialData/Restoration/low_rr.shp")
write_sf(low_trees, "./Inputs/SpatialData/Restoration/low_rr.kml")

write_sf(high_trees, "./Inputs/SpatialData/Restoration/high_rr.shp")
write_sf(high_trees, "./Inputs/SpatialData/Restoration/high_rr.kml")


ml <- fread("D:/Sync/BVRC/WBP program/Restoration/Seed Collection/2022 BVRC tree screening candidates.csv",
            na.strings = "n/a")
ml <- ml[!is.na(easting)]
ml_9u <- st_as_sf(ml, coords = c("easting","northing"), crs = 3156)
ml_9u_LL <- st_transform(ml_9u, crs = "+proj=longlat +datum=NAD83")
ml_9u_LL_dt <- as.data.table(cbind(ml_9u_LL %>% 
                                     select(Unique_ID),st_coordinates(ml_9u_LL)))
ml_9u_LL_dt[,geometry:=NULL]
ml_9u_LL_dt


all_coll_22 <- read_sf("./Inputs/SpatialData/Restoration/Caging2022/AllSites_Caging2022.shp")


plot(st_coordinates(st_transform(all_coll_22,crs = "+proj=longlat +datum=NAD83")))

all_coll_22_dt <- data.table(all_coll_22)
all_coll_22_dt <- cbind(all_coll_22_dt[,.(Name)],
                        st_coordinates(st_transform(all_coll_22,crs = "+proj=longlat +datum=NAD83")))
write.csv(all_coll_22_dt, "D:/Sync/BVRC/WBP program/Restoration/Seed Collection/Data/AllSites_Caging2022.csv",
          row.names = FALSE)