#### Download community data from SeedClim database ####

#load packages
library("tidyverse")
library("DBI")# also needs RSQLite installed


## ---- load_community

#make database connection
con <- dbConnect(RSQLite::SQLite(), "database/seedclim2020.sqlite")

#load cover data and metadata
#cover
#source("R/importSource/loadCover.r")

#subturf frequencies
#source("R/importSource/loadSubplotfreq.r")

#get taxonomy table
taxa <- tbl(con, "taxon") %>%
  collect()

## tells you whats in the data base
DBI::dbListTables(con)

## 2. Assemble community data set for the turf
comdat_TTC <- tbl(con, "turf_community") %>%
  collect() %>%
  left_join(tbl(con, "taxon"),by="species", copy=TRUE) %>%
  left_join(tbl(con, "turfs"), by="turfID", copy=TRUE) %>%
  left_join(tbl(con, "plots"), by=c("destinationPlotID"="plotID"), copy=TRUE) %>% 
  left_join(tbl(con, "blocks"), by= "blockID", copy=TRUE, suffix = c("", "_block")) %>%
  left_join(tbl(con, "sites"), by="siteID", copy=TRUE) %>%
  left_join(tbl(con, "turf_environment"), by = c("turfID", "year"), copy = TRUE) %>% 
  select(siteID, blockID, turfID, latitude, longitude, annual_precipitation_gridded, summer_temperature_gridded, year,
         TTtreat, species, species_name, family, cover, total_vascular, total_bryophytes, total_lichen, vegetation_height, moss_height) %>%
  collect() %>% 
  filter(TTtreat =="TTC")

# take out unidentified species and rename species to fit trait data
comdat_TTC <- filter(comdat_TTC, !(species %in% c("NID.seedling", "NID.gram","Åkerplante","NID.herb"))) %>% 
  mutate(species=gsub("\\.", "_", species))


write.csv(comdat_TTC, "comdat_2009-2019_TTC.csv")

