#################################
##    INCLINE data cleaning    ##
#################################

# Script by Eva Lieungh and Ragnhild Gya

#setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/Paper 3 community dispersal interactions') # Evas directory
#setwd() # Ragnhilds directory

#### Libraries ####
library("tidyverse")
#library("lubridate")
#library("mosaic")

#### Load community data ####
community <- read.csv('Data fra Ragnhild/INCLINE_community_2018_2019.csv', sep = ';') # 8676 obs. of 164 variables
str(community)

#### Clean and reorganise data ####

## make a new column with unique plot ID from Site,block and plot ID
community <- community %>%
  unite(plotID, c(Site,Block,plot), remove = FALSE)

## cover data
cover <- community %>%
  filter(Site!="")%>% # remove any lines with no Site
  mutate(Site= substr(Site, 1,3))%>% # Shorten site name to first three letters
  filter(Measure == "cover") # keep just the cover data

cover <- cover %>% # not exactly sure what this chunk of code does, copied from Cleaning2 script
  select(-subPlot, -Measure, -recorder, -Nid.seedling, -lichen, -litter, -soil, -rock) %>% 
  gather(species, cover, Ach_mil:Vio_riv)%>%
  mutate(cover = as.numeric(cover))%>%
  filter(!is.na(cover))%>%  #Takes out the species that are not present in the dataset
  mutate(species=gsub("\\.", "_", species))%>%
  mutate(Site = as.character(Site, levels = c("Ulv", "Lav", "Gud", "Skj")))

cover <- cover %>% # find the mean cover by plot (or by site)
  group_by(Site, species)%>%
  mutate(mean_cover=mean(cover, na.rm=TRUE))%>% #If you want the turf data use mutate, if you want the site data use summarise
  ungroup()%>%
  mutate(Site = factor(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs")))%>%
  group_by(turfID)%>%
  mutate(sum_cover= sum(cover))%>%
  mutate(cover_species=cover/sum_cover*100) %>% 
  filter(! turfID == "Lav1XC") #Only has one species in it

## community (subPlot) data
community <- community %>%
  filter(Site!="")%>% # remove any lines with no Site
  mutate(Site= substr(Site, 1,3))%>% # Shorten site name to first three letters
  filter(Measure == "subPlot") # keep just the subPlot data


## transform data from broad to long format
com.long <- gather()