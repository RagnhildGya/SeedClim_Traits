#### Libraries ####
library("tidyverse")
library("lubridate")
library("janitor")

#### Load trait data ####

traits <-read.csv("Data/LeafTraits_SeedClim.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)

dict_Site_2016 <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
                               "old siteID
Arh Arhelleren
Ovs Ovstedalen
Ves Veskre
Skj Skjelingahaugen
Lav Lavisdalen
Gud Gudmedalen
Ulv Ulvehaugen
Vik Vikesland
Hog Hogsete
Alr Alrust
Fau Fauske
Ram Rambera")


#### Cleaning the trait data ####

traits <- traits |> 
  rename(Height=Height..mm., Lth_1=Lth.1..mm., Lth_2= Lth.2..mm., Lth_3= Lth.3..mm., Wet_mass=Wet.mass..g., Dry_mass=Dry.mass..g., Site=Location) |>  #Renaming weird named columns
  select(-Lth.average..mm.) |>  #removing this column, as we make it again later
  mutate(Date = mdy(Date)) |>  #formating the date column
  mutate(Site = factor(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"))) |>  #Ordering the sites from cold to warm and dry to wet
  mutate(Dry_mass = replace(Dry_mass, Dry_mass < 0.0005, NA)) |>  #Replacing anything that us under 0.0005 grams with NA because these values are so low and not very trustworthy (outside of the margin of error of the balance)
  mutate(Wet_mass = replace(Wet_mass, Wet_mass < 0.0005, NA)) |>  #
  mutate(Lth_3 = replace(Lth_3, Lth_3 < 0.9, NA)) |> #One outlier that is very far off measurment 1 and 2, replacing with NA.
  filter(!is.na(Site)) #Remove the empty rows
  
  
#### Load leaf area data ####

LA <- read.csv2("Data/Leaf_area_total.csv", stringsAsFactors = FALSE)

LA<-transform(LA, Leaf_area = as.numeric(Leaf_area))

LA <- LA |> 
  filter(Leaf_area > 0.1)


#### Merge the trait data and the leaf area data ####

traitdata <- traits |> 
  mutate(ID = paste0(Site, "_", Species, "_", Individual, ".jpg")) |> 
  mutate(Site_sp=paste0(Site,"_", Species)) |> 
  full_join(LA, by=c("ID"="Image_file"))


#### Load CN data ####

CN <- read.csv2("Data/CNratio.csv", dec=".", sep=";")

#Making a dictionary for the CN name abreviations

dict_CN <- read.csv2("Data/Dict_CN.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)

#Making a dictionary for the site names in the CN file

dict_Site <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
  "old Site
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



CN<-CN |> 
  mutate(site_abr = substr(Name, 1,2)) |> 
  mutate(species_abr = substr(Name, 3,6)) |> 
  mutate(Individual = substr(Name, 7,8)) |> 
  left_join(dict_CN, by = c("species_abr" = "CN_ab")) |> 
  left_join(dict_Site, by = c("site_abr" = "old")) |> 
  mutate(ID = paste0(Site, "_", Species, "_", Individual, ".jpg")) |>  
  filter(!(Name=="VECAR101")) |>  #Because it was a to small sample to get good data from it
 select(-Humidity.., -Name, -Weight, -Method, -N.Factor, -C.Factor, -N.Blank, -C.Blank, -Memo, -Info, -Date..Time, -N.Area, -C.Area) |>  
  rename(C_percent=C.., N_percent = N.., CN_ratio = CN.ratio) |>  
  mutate(Site = factor(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"))) |> 
  group_by(ID) |> #Some individuals had two values, making a mean of those values as I assume these individuals where measured twice in the lab.
  mutate(C_percent = mean(C_percent),
         N_percent = mean(N_percent),
         CN_ratio = mean(CN_ratio)) |> 
  unique()


#### Merge the trait data and the CN data ####

traitdata <- traitdata |> 
  full_join(CN, by=c("ID"="ID", "Site"="Site", "Species"="Species", "Individual"="Individual"))

#### Remove data points for different reasons ###

traitdata <- traitdata |>  
  filter(!(Species=="Agr_cap" & Site =="Alr" & Individual=="9")) |>   #Looks weird from picture
  filter(!(ID=="Ves_Leo_aut_6.jpg")) #Looks damaged from scan of leaf area
#filter(!(Species=="Hyp_mac" & Site=="Alr")) |>  #They were collected inside of the fence
#filter(!(Species=="Car_vag" & Site == "Ves"))|>  #Less then 4 individuals
#filter(!(Species=="Fes_rub" & Site == "Ulv"))|>  #Less then 4 individuals
#filter(!(Species=="Fes_rub" & Site == "Gud"))|>  #Less then 4 individuals
#filter(!(Species=="Hie_pil" & Site == "Gud"))|>  #Less then 4 individuals
#filter(!(Species=="Pot_cra" & Site == "Gud"))|>  #Less then 4 individuals
#filter(!(Species=="Ran_acr" & Site == "Skj"))|>  #Less then 4 individuals
#filter(!(Species=="Hie_pil" & Site == "Gud"))|>  #Less then 4 individuals
#filter(!(Species=="Vac_myr" & Site == "Ves"))|>  #Less then 4 individuals
#filter(!(Species=="Ver_alp" & Site == "Ves")) |>   #Less then 4 individuals

### Calculate traits and transform traits ###

traitdata <- traitdata |>  
  mutate(SLA = Leaf_area/Dry_mass) |> 
  mutate(LDMC = Dry_mass/Wet_mass)|> 
  mutate(Lth_ave = rowMeans(select(traitdata, starts_with("Lth")), na.rm = TRUE)) |>  #Make the numbers only with four digits
  select(-Lth_1, -Lth_2, -Lth_3) |>  
  filter(!LDMC > 1)

traitdata_trans <- traitdata |>  
  mutate(Height = log(Height),
         Wet_mass = log(Wet_mass),
         Dry_mass = log(Dry_mass),
         Leaf_area = log(Leaf_area)) |>  
  tidyr::gather(Trait, Value, Height, Wet_mass, Dry_mass, Leaf_area, N_percent, C_percent, CN_ratio, SLA, LDMC, Lth_ave) |>  
  mutate(Trait = recode(Trait, "Leaf_area" = "Leaf_Area_cm2",
                        "SLA" = "SLA_cm2_g",
                        "Dry_mass" = "Dry_Mass_g",
                        "Wet_mass" = "Wet_Mass_g",
                        "Lth_ave" = "Leaf_Thickness_Ave_mm",
                        "Height" = "Plant_Height_mm")) |>  
  mutate(Trait_trans = recode(Trait,
                              "Plant_Height_mm" = "Plant_Height_mm_log",
                              "Wet_Mass_g" = "Wet_Mass_g_log",
                              "Dry_Mass_g" = "Dry_Mass_g_log",
                              "Leaf_Area_cm2" = "Leaf_Area_cm2_log"))


#### Add info about species ####


systematics_species<- read.csv2("Data/systematics_species.csv", sep=";", stringsAsFactors = FALSE)

species_info<- read.csv2("Data/species_info.csv", sep=";", stringsAsFactors = FALSE)

species_info <- species_info|> 
  select(species, functionalGroup, lifeSpan, occurrence) |>  
  mutate(species = gsub("\\.", "_", species))



traitdata_1 <- traitdata_trans |> 
  left_join(systematics_species, by = c("Species"="Species"))

traitdata_1 <- traitdata_1 |> 
  left_join(species_info, by =c("Species" = "species")) |>  
  select(-Image, -Comment, -ID, -Site_sp) |>  
  left_join(dict_Site_2016, by = c("Site" = "old")) |> 
  mutate(Temp_level = as.factor(recode(siteID, Ulvehaugen = 6.5, Lavisdalen = 6.5,  Gudmedalen = 6.5, Skjelingahaugen = 6.5, Alrust = 8.5, Hogsete = 8.5, Rambera = 8.5, Veskre = 8.5, Fauske = 10.5, Vikesland = 10.5, Arhelleren = 10.5, Ovstedalen = 10.5)),
         Precip_level = as.factor(recode(siteID, Ulvehaugen = 600, Alrust = 600, Fauske = 600, Lavisdalen = 1200, Hogsete = 1200, Vikesland = 1200, Gudmedalen = 2000, Rambera = 2000, Arhelleren = 2000, Skjelingahaugen = 2700, Veskre = 2700, Ovstedalen = 2700)))



#### Community SeedClim data ####

# Reading in and cleaning the community data 

#Old community data was community <-read.csv2("Data/comdat_TTC.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
community <-read.csv2("Data/comdat_2009-2019_TTC.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

community <- community |> 
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         annualPrecipitation_gridded = as.numeric(annual_precipitation_gridded),
         summerTemperature_gridded = as.numeric(summer_temperature_gridded),
         cover = as.numeric(cover),
         total_vascular = as.numeric(total_vascular),
         total_bryophytes = as.numeric(total_bryophytes),
         vegetation_height = as.numeric(vegetation_height),
         moss_height = as.numeric(moss_height)) |>  
  mutate(Site= substr(siteID, 1,3)) |>  
  select(-X) |>  
  mutate(Site = as.character(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"))) |>  
  mutate(Temp_level = as.factor(recode(Site, Ulv = 6.5, Lav = 6.5,  Gud = 6.5, Skj = 6.5, Alr = 8.5, Hog = 8.5, Ram = 8.5, Ves = 8.5, Fau = 10.5, Vik = 10.5, Arh = 10.5, Ovs = 10.5)),
         Precip_level = as.factor(recode(Site, Ulv = 600, Alr = 600, Fau = 600, Lav = 1200, Hog = 1200, Vik = 1200, Gud = 2000, Ram = 2000, Arh = 2000, Skj = 2700, Ves = 2700, Ovs = 2700)))

community <- community |> 
  left_join(systematics_species, by = c("species"="Species")) |>  
  left_join(species_info, by = c("species" = "species")) 

## Environmental data ##

 env_old <- community |>  
   select(siteID, annualPrecipitation_gridded, summerTemperature_gridded) |>  
   mutate(Site = substr(siteID, 1,3)) |> 
   distinct() |>  
   mutate(Temp_century = summerTemperature_gridded,
          Precip_century = annualPrecipitation_gridded,
          Temp_level = as.factor(recode(Site, Ulv = 6.5, Lav = 6.5,  Gud = 6.5, Skj = 6.5, Alr = 8.5, Hog = 8.5, Ram = 8.5, Ves = 8.5, Fau = 10.5, Vik = 10.5, Arh = 10.5, Ovs = 10.5)),
          Precip_level = as.factor(recode(Site, Ulv = 600, Alr = 600, Fau = 600, Lav = 1200, Hog = 1200, Vik = 1200, Gud = 2000, Ram = 2000, Arh = 2000, Skj = 2700, Ves = 2700, Ovs = 2700))) |>  
   select(Site, Temp_century, Precip_century, Temp_level, Precip_level) |>  
   left_join(dict_Site_2016, by = c("Site" = "old"))

 env <- read.csv("Data/AAGriddedDailyClimateData2009-2019.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE)
 
 env <- env |>  
   mutate(year = year(date),
          month = month(date),
          day = day(date)) |>  
   filter(variable %in% c("temperature", "precipitation")) |>  
   pivot_wider(names_from = variable, values_from = value)
 
 #Creating a variable of last years summer temperature
 
 summer_prev <- env |>  
   group_by(siteID, year) |>  
   filter(month %in% c(6:9)) |>  
   mutate(Temp_yearly_prev = mean(temperature)) |>  
   select(siteID, year, Temp_yearly_prev) |>  
   mutate(year = year + 1) |>  
   unique()
 
 #Creating a variable of this years summer temperature
 
 summer <- env |>  
   group_by(siteID, year) |>  
   filter(month %in% c(6:9)) |>  
   mutate(Temp_summer = mean(temperature)) |>  
   select(siteID, year, Temp_summer) |>  
   unique()
 
 env <- env |>  
   mutate(year2 = ifelse(month > 7, year + 1, year)) |>   #Creating a variable that allows me to divide the year from fieldseason to fieldseason rather than january-january 
   group_by(siteID, year2) |>  
   mutate(Precip_yearly = sum(precipitation)) |>  #Creating variable for precipitation 1 year back from the sampling date (july-july) by grouping by year2
   ungroup() |> 
   group_by(siteID, year) |> 
   filter(month %in% c(5:8)) |>  
   mutate(Temp_yearly_tetra = mean(temperature)) |>  
   filter(month %in% c(5:7)) |>  
   mutate(Temp_yearly_spring = mean(temperature),
          Precip_yearly_spring = sum(precipitation)) |>  #Creating variable for that years summer temperature (we called it spring because we chose to not include the part of the summer after data collection, so we end it 31. july)
   ungroup() |>  
   select(siteID, year, Precip_yearly, Temp_yearly_spring, Precip_yearly_spring, Temp_yearly_tetra) |>  
   unique() |>  
   left_join(y = summer_prev, by = c("siteID" = "siteID", "year" = "year")) |>  
   left_join(y = summer, by = c("siteID" = "siteID", "year" = "year")) |>  
   ungroup() |>  
   group_by(siteID) |>  
   left_join(y = env_old, by = c("siteID" = "siteID")) |>  
   mutate(Temp_decade_figure = mean(Temp_yearly_tetra, na.rm = TRUE),
          Temp_se_figure = (sd(Temp_yearly_tetra, na.rm = TRUE) / sqrt(length(Temp_yearly_tetra))),
          Temp_decade = mean(Temp_yearly_spring, na.rm = TRUE),
          Temp_se = (sd(Temp_yearly_spring, na.rm = TRUE) / sqrt(length(Temp_yearly_spring))),
          Precip_decade = mean(Precip_yearly),
          Precip_se = (sd(Precip_yearly) / sqrt(length(Precip_yearly)))) |>  
   mutate(Temp_annomalies = Temp_yearly_spring - Temp_century,
          Precip_annomalies = Precip_yearly - Precip_century) |>  
   mutate(Temp_level = fct_relevel(Temp_level, c("6.5", "8.5", "10.5"))) |>  
   mutate(Precip_level = fct_relevel(Precip_level, c("600", "1200", "2000", "2700"))) |>  
   ungroup()
 
#### Making a table of the old and new climate variables ###
 Vestland_climate_grid <- env |>  
   select(siteID, Temp_century, Precip_century, Temp_decade_figure, Precip_decade) |>  
   unique() |>  
   rename("Temp_1960_1990" = Temp_century, "Precip_1960_1990" = Precip_century, "Temp_2009_2019" = Temp_decade_figure, "Precip_2009_2019" = Precip_decade) |>
   mutate(Temp_1960_1990 = round(Temp_1960_1990, digits = 2), Temp_2009_2019 = round(Temp_2009_2019, digits = 2), Precip_1960_1990 = round(Precip_1960_1990, digits = 0), Precip_2009_2019 = round(Precip_2009_2019, digits = 0))
 
 rm(CN)
 rm(dict_CN)
 rm(dict_Site)
 rm(env_old)
 rm(LA)
 rm(species_info)
 rm(systematics_species)
 rm(traits)
 rm(traitdata)
 rm(traitdata_trans)
 rm(dict_Site_2016)