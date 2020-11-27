#### Libraries ####
library("tidyverse")
library("lubridate")
#library("mosaic")
library(forcats)

#### Load trait data ####

traits <-read.csv("Data/LeafTraits_SeedClim.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)


#### Cleaning the trait data ####

traits <- traits %>%
  rename(Height=Height..mm., Lth_1=Lth.1..mm., Lth_2= Lth.2..mm., Lth_3= Lth.3..mm., Wet_mass=Wet.mass..g., Dry_mass=Dry.mass..g., Site=Location) %>% #Renaming weird named columns
  select(-Lth.average..mm.) %>% #removing this column, as we make it again later
  mutate(Date = mdy(Date)) %>% #formating the date column
  mutate(Site = factor(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"))) %>% #Ordering the sites from cold to warm and dry to wet
  mutate(Dry_mass = replace(Dry_mass, Dry_mass < 0.0005, NA)) %>% #Replacing anything that us under 0.0005 grams with NA because these values are so low and not very trustworthy (outside of the margin of error of the balance)
  mutate(Wet_mass = replace(Wet_mass, Wet_mass < 0.0005, NA)) %>% #
  mutate(Lth_3 = replace(Lth_3, Lth_3 < 0.9, NA)) #One outlier that is very far off measurment 1 and 2, replacing with NA.
  
  
#### Load leaf area data ####

LA <- read.csv2("Data/Leaf_area_total.csv", stringsAsFactors = FALSE)

LA<-transform(LA, Leaf_area = as.numeric(Leaf_area))

LA <- LA %>%
  filter(Leaf_area > 0.1)


#### Merge the trait data and the leaf area data ####

traitdata <- traits %>%
  mutate(ID = paste0(Site, "_", Species, "_", Individual, ".jpg")) %>%
  mutate(Site_sp=paste0(Site,"_", Species)) %>%
  full_join(LA, by=c("ID"="Image_file"))


#### Load CN data ####

CN <- read.csv2("Data/CNratio.csv", dec=".", sep=";")

#Making a dictionary for the CN name abreviations

dict_CN <- read.csv2("Data/Dict_CN.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)

#Making a dictionary for the site names in the CN file

dict_Site <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
  "old new
  AR Arh
  OV Ovs
  VE Ves
  SK Skj
  LA Lav
  GU Gud
  UL Ulv
  VI Vik
  HO Hog
  AL Alr
  FA Fau
  RA Ram")



CN<-CN %>%
  mutate(Site= substr(Name, 1,2)) %>%
  mutate(Species = substr(Name, 3,6)) %>%
  mutate(Individual = substr(Name, 7,8)) %>%
  mutate(Species = plyr::mapvalues(Species, from = dict_CN$CN_ab, to = dict_CN$Species)) %>%
  mutate(Site = plyr::mapvalues(Site, from = dict_Site$old, to = dict_Site$new)) %>%
  mutate(ID = paste0(Site, "_", Species, "_", Individual, ".jpg")) %>%
  filter(!(Name=="VECAR101")) %>% #Because it was a to small sample to get good data from it
 select(-Humidity.., -Name, -Weight, -Method, -N.Factor, -C.Factor, -N.Blank, -C.Blank, -Memo, -Info, -Date..Time, -N.Area, -C.Area) %>% 
  rename(C_percent=C.., N_percent = N.., CN_ratio = CN.ratio) %>% 
  mutate(Site = factor(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs")))


#### Merge the trait data and the CN data ####

traitdata <- traitdata %>%
  full_join(CN, by=c("ID"="ID", "Site"="Site", "Species"="Species", "Individual"="Individual"))

#### Remove data points for different reasons ###

traitdata <- traitdata %>% 
  filter(!(Species=="Agr_cap" & Site =="Alr" & Individual=="9")) %>%  #Looks weird from picture
  filter(!(ID=="Ves_Leo_aut_6.jpg")) #Looks damaged from scan of leaf area
#filter(!(Species=="Hyp_mac" & Site=="Alr")) %>% #They were collected inside of the fence
#filter(!(Species=="Car_vag" & Site == "Ves"))%>% #Less then 4 individuals
#filter(!(Species=="Fes_rub" & Site == "Ulv"))%>% #Less then 4 individuals
#filter(!(Species=="Fes_rub" & Site == "Gud"))%>% #Less then 4 individuals
#filter(!(Species=="Hie_pil" & Site == "Gud"))%>% #Less then 4 individuals
#filter(!(Species=="Pot_cra" & Site == "Gud"))%>% #Less then 4 individuals
#filter(!(Species=="Ran_acr" & Site == "Skj"))%>% #Less then 4 individuals
#filter(!(Species=="Hie_pil" & Site == "Gud"))%>% #Less then 4 individuals
#filter(!(Species=="Vac_myr" & Site == "Ves"))%>% #Less then 4 individuals
#filter(!(Species=="Ver_alp" & Site == "Ves")) %>%  #Less then 4 individuals

### Calculate traits and transform traits ###

traitdata <- traitdata %>% 
  mutate(SLA = Leaf_area/Dry_mass) %>%
  mutate(LDMC = Dry_mass/Wet_mass)%>%
  mutate(Lth_ave = rowMeans(select(traitdata, starts_with("Lth")), na.rm = TRUE)) %>% #Make the numbers only with four digits
  #filter(LDMC<1) %>% 
  select(-Lth_1, -Lth_2, -Lth_3)

traitdata_trans <- traitdata %>% 
  mutate(Height = log(Height),
         Wet_mass = log(Wet_mass),
         Dry_mass = log(Dry_mass),
         Leaf_area = log(Leaf_area)) %>% 
  gather(Trait, Value, Height, Wet_mass, Dry_mass, Leaf_area, N_percent, C_percent, CN_ratio, SLA, LDMC, Lth_ave) %>% 
  mutate(Trait = recode(Trait, "Leaf_area" = "Leaf_Area_cm2",
                        "SLA" = "SLA_cm2_g",
                        "Dry_mass" = "Dry_Mass_g",
                        "Wet_mass" = "Wet_Mass_g",
                        "Lth_ave" = "Leaf_Thickness_Ave_mm",
                        "Height" = "Plant_Height_mm")) %>% 
  mutate(Trait_trans = recode(Trait,
                              "Plant_Height_mm" = "Plant_Height_mm_log",
                              "Wet_Mass_g" = "Wet_Mass_g_log",
                              "Dry_Mass_g" = "Dry_Mass_g_log",
                              "Leaf_Area_cm2" = "Leaf_Area_cm2_log"))


#### Add info about species ####


systematics_species<- read.csv2("Data/systematics_species.csv", sep=";", stringsAsFactors = FALSE)

species_info<- read.csv2("Data/species_info.csv", sep=";", stringsAsFactors = FALSE)

species_info <- species_info%>%
  select(species, functionalGroup, lifeSpan, occurrence)%>%
  mutate(species=gsub("\\.", "_", species))



traitdata_1 <- traitdata_trans %>%
  left_join(systematics_species, by = c("Species"="Species"))

traitdata_1 <- traitdata_1 %>%
  left_join(species_info, by =c("Species" = "species"))

#### Community SeedClim data ####

# Reading in and cleaning the community data 

community <-read.csv2("Data/comdat_TTC.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

community <- community %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         annualPrecipitation_gridded = as.numeric(annualPrecipitation_gridded),
         summerTemperature_gridded = as.numeric(summerTemperature_gridded),
         cover = as.numeric(cover)) %>% 
  mutate(Site= substr(siteID, 1,3)) %>% 
  select(-X) %>% 
  mutate(Site = as.character(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs")))

community <- community %>%
  left_join(systematics_species, by = c("species"="Species"))

## Environmental data ##

 env_old <- community %>% 
   select(siteID, annualPrecipitation_gridded, summerTemperature_gridded) %>% 
   mutate(Site = substr(siteID, 1,3)) %>%
   distinct() %>% 
   mutate(Temp_century = summerTemperature_gridded,
          Precip_century = annualPrecipitation_gridded,
          Temp_level = as.factor(recode(Site, Ulv = 6.5, Lav = 6.5,  Gud = 6.5, Skj = 6.5, Alr = 8.5, Hog = 8.5, Ram = 8.5, Ves = 8.5, Fau = 10.5, Vik = 10.5, Arh = 10.5, Ovs = 10.5)),
          Precip_level = as.factor(recode(Site, Ulv = 600, Alr = 600, Fau = 600, Lav = 1200, Hog = 1200, Vik = 1200, Gud = 2000, Ram = 2000, Arh = 2000, Skj = 2700, Ves = 2700, Ovs = 2700))) %>% 
   select(Site, Temp_century, Precip_century, Temp_level, Precip_level)

 env <-read.csv("Data/GriddedDailyClimateData2009-2019.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE)
 
 env <- env %>% 
   group_by(Site, Year) %>% 
   mutate(Precip_yearly = sum(Precipitation)) %>% 
   filter(Month %in% c(6:9)) %>% 
   mutate(Temp_yearly = mean(Temperature)) %>% 
   select(Site, Year, Precip_yearly, Temp_yearly) %>% 
   unique() %>% 
   ungroup() %>% 
   group_by(Site) %>% 
   mutate(Temp_decade = mean(Temp_yearly),
          Temp_se = (sd(Temp_yearly) / sqrt(length(Temp_yearly))),
          Precip_decade = mean(Precip_yearly),
          Precip_se = (sd(Precip_yearly) / sqrt(length(Precip_yearly)))) %>% 
   mutate(Temp_deviation_decade = Temp_yearly - Temp_decade,
          Precip_deviation_decade = Precip_yearly - Precip_decade) %>% 
   left_join(y = env_old, by = "Site") %>% 
   mutate(Temp_level = fct_relevel(Temp_level, c("6.5", "8.5", "10.5"))) %>% 
   mutate(Precip_level = fct_relevel(Precip_level, c("600", "1200", "2000", "2700")))
 

####################  Old code #####################
### Code to calculate community weighted means.  ###
### Also to look for mistakes and filter out     ###
### 80 % community etc.                          ###
####################################################

#### Checking that I have 85% of the community ####

# check_community_df <- wcommunity_df %>%
#   group_by(Site, Species, turfID)%>%
#   select(Site, turfID, Species, cover, SLA_mean, Lth_mean, Height_mean, LDMC_mean, LA_mean, CN_ratio_mean, sum_cover)%>%
#   unique()%>%
#   ungroup()%>%
#   group_by(turfID)%>%
#   mutate(cover_traits = (sum(cover)))%>%
#   filter(!is.na(SLA_mean))%>%
#   mutate(community_covered_trait=cover_traits/sum_cover*100)
#
#
# uncomplete_turf <- check_community_df%>%
#   filter(community_covered_trait<80)%>%
#   distinct(turfID, .keep_all=TRUE)
#
# Uncomplete_turfs<-as.vector(uncomplete_turf$turfID)
#
# complete_turf <- check_community_df%>%
#   filter(community_covered_trait>70)%>%
#   distinct(turfID, .keep_all=TRUE)
#
# Complete_turfs<-as.vector(complete_turf$turfID)
# 
# #### Filtering out turfs with less than 70% of the community present ###
# 
# check_community_df <- wcommunity %>%
#   group_by(Site, Species, turfID)%>%
#   select(Site, turfID, Species, cover, SLA_mean, Lth_mean, Height_mean, LDMC_mean, LA_mean, CN_ratio_mean, sum_cover)%>%
#   unique()%>%
#   ungroup()%>%
#   group_by(turfID)%>%
#   mutate(cover_traits = (sum(cover)))%>%
#   filter(!is.na(SLA_mean))%>%
#   mutate(community_covered_trait=cover_traits/sum_cover*100)
# 
# complete_turf <- check_community_df%>%
#   filter(community_covered_trait>70)%>%
#   distinct(turfID, .keep_all=TRUE)
# 
# Complete_turfs<-as.vector(complete_turf$turfID)
#   
# wcommunity_df <- filter(wcommunity, turfID %in% Complete_turfs)

