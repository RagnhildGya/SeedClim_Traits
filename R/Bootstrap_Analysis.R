#### Bottstrapping and analysis for trait driver theory paper with climate in time and space ####

#### Source ####

source("R/Cleaning.R")
#source("R/Bootstraping.R")

#### Libraries ####

library(broom.mixed)
library(lme4)
library(lmerTest)
library(purrr)
library(piecewiseSEM)
library(factoextra)
# library(GGally)
library(ggcorrplot)
# library(textshape)
library(traitstrap)
library(vegan)
library(ggvegan)
#library(drake)
#library(default)
#library(conflicted)

set.seed(47)

#### Setting conflict standards ####

# conflict_prefer("map", winner="purrr")
# conflict_prefer("filter", winner = "dplyr")


#### Making data ready for traitstrap and merging ####

## Community ##

community_for_boostrapping <- community %>% 
  filter(!year == "2010") %>% 
  select(siteID, blockID, turfID, year, species, Full_name, Genus, Family, Order, cover)

community_for_analysis <- community %>% 
  filter(!year == "2010") %>% 
  select(siteID, blockID, turfID, year, species, Full_name, Genus, Family, Order, cover, total_vascular, total_bryophytes,vegetation_height, moss_height, functionalGroup)

turf_site_dict <- community %>% 
  select(siteID, turfID) %>% 
  distinct()

rm(community)
## Trait data ##

traitdata_2 <- traitdata_1 %>% 
  mutate(blockID = "",
         turfID = "") 

rm(traitdata_1)

## Climate data - transform precipitation from mm to m ##

env <- env %>% 
  mutate(Precip_yearly = Precip_yearly/1000,
         Precip_yearly_spring = Precip_yearly_spring/1000,
         Precip_decade = Precip_decade/1000,
         Precip_se = Precip_se/1000,
         Precip_annomalies = Precip_annomalies/1000,
         Precip_century = Precip_century/1000)

## Climate data - making model for mean change in temp and precip ##

temp_model <- lmer(Temp_yearly_spring ~ Year + (1|Site), data = env)
precip_model <- lmer(Precip_yearly ~ Year + (1|Site), data = env)

# summary(temp_model)
# rsquared(temp_model)
# summary(precip_model)
# rsquared(precip_model)

env_predictions <-function(model_temp, model_precip) {
  
  newdata <- expand.grid(Year=c(2009, 2011, 2012, 2013, 2015, 2016, 2017, 2019), Site = c("Alr", "Arh", "Fau", "Gud", "Hog", "Lav", "Ovs", "Ram", "Skj", "Ulv", "Ves", "Vik"))
  
  newdata$temp_modeled <- predict(object = model_temp, newdata = newdata, re.form = NULL, allow.new.levels=TRUE)
  newdata$precip_modeled <- predict(object = model_precip, newdata = newdata, re.form = NULL, allow.new.levels=TRUE)
  
  return(newdata)
}

env_pred <- env_predictions(temp_model, precip_model) %>% 
  mutate(Site = as.character(Site))


env <- env %>% 
  left_join(env_pred, by = c("Site" = "Site", "Year" = "Year"))

#### Bootstrapping ####

Trait_impute_per_year <- function(com_dat, trait_dat){
  
  SeedClim_traits <- trait_np_bootstrap(trait_impute(comm = com_dat,
                                                     traits = trait_dat, 
                                                     scale_hierarchy = c("siteID", "blockID", "turfID"),
                                                     global = TRUE,
                                                     taxon_col = c("Full_name", "Genus", "Family"),
                                                     trait_col = "Trait_trans",
                                                     value_col = "Value",
                                                     other_col = "year",
                                                     abundance_col = "cover"))
  
  return(SeedClim_traits)
}


Imputed_traits_fullcommunity <- Trait_impute_per_year(com_dat = community_for_boostrapping, trait_dat = traitdata_2)

sum_moments_fullcommunity <- trait_summarise_boot_moments(Imputed_traits_fullcommunity)

#traitstrap:::autoplot.imputed_trait(Imputed_traits_fullcommunity) 
#traitstrap:::autoplot.imputed_trait(Imputed_traits_without_intra) 


Trait_impute_without_intraA <- function(com_dat, trait_dat){
  
  trait_dat <- trait_dat %>% 
    mutate(turfID = "")
  
  com_dat <- com_dat %>% 
    filter(siteID %in% c("Hogsete", "Ulvehaugen", "Vikesland", "Gudmedalen", "Rambera", "Arhelleren"))
  
  SeedClim_traits <- trait_np_bootstrap(trait_impute(comm = com_dat,
                                                     traits = trait_dat, 
                                                     scale_hierarchy = "turfID",
                                                     taxon_col = c("Full_name", "Genus", "Family"),
                                                     trait_col = "Trait_trans",
                                                     value_col = "Value",
                                                     other_col = "year",
                                                     abundance_col = "cover")) 
  
  return(SeedClim_traits)
}

Trait_impute_without_intraB <- function(com_dat, trait_dat){
  
  trait_dat <- trait_dat %>% 
    mutate(turfID = "")
  
  com_dat <- com_dat %>% 
    filter(siteID %in% c("Skjelingahaugen", "Veskre", "Ovstedalen", "Alrust", "Fauske", "Lavisdalen"))
  
  SeedClim_traits <- trait_np_bootstrap(trait_impute(comm = com_dat,
                                                     traits = trait_dat, 
                                                     scale_hierarchy = "turfID",
                                                     taxon_col = c("Full_name", "Genus", "Family"),
                                                     trait_col = "Trait_trans",
                                                     value_col = "Value",
                                                     other_col = "year",
                                                     abundance_col = "cover")) 
  
  return(SeedClim_traits)
}

Imputed_traits_without_intraA <- Trait_impute_without_intraA(com_dat = community_for_boostrapping, trait_dat = traitdata_2)
Imputed_traits_without_intraB <- Trait_impute_without_intraB(com_dat = community_for_boostrapping, trait_dat = traitdata_2)
Imputed_traits_without_intra = bind_rows(Imputed_traits_without_intraA, Imputed_traits_without_intraB)

sum_moments_without_intraA <- trait_summarise_boot_moments(Imputed_traits_without_intraA)
sum_moments_without_intraB <- trait_summarise_boot_moments(Imputed_traits_without_intraB)
sum_moment_without_intra = bind_rows(sum_moments_without_intraA, sum_moments_without_intraB)

#### Adding climate info & pivoting longer ####

sum_moments_climate_fullcommunity = bind_rows(
  sum_moments_fullcommunity %>% 
    left_join(env, by = c("siteID" = "siteID", "year" = "Year")))

sum_moments_climate_without_intra = bind_rows(
  sum_moment_without_intra %>% 
    left_join(turf_site_dict, by = c("turfID" = "turfID")) %>% 
    left_join(env, by = c("siteID" = "siteID", "year" = "Year")))


moments_clim_long_fullcommunity <- Imputed_traits_fullcommunity %>% 
  pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value") %>% 
  left_join(env, by = c("siteID" = "siteID", "year" = "Year"))


moments_clim_long_without_intra <- Imputed_traits_without_intra %>% 
  pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value") %>% 
  left_join(turf_site_dict, by = c("turfID" = "turfID")) %>% 
  left_join(env, by = c("siteID" = "siteID", "year" = "Year"))


###### Mixed effect model testing ######

#### Making dataset for models ####

# With intraspecific variability
memodel_data_fullcommunity <- moments_clim_long_fullcommunity %>% 
  ungroup() %>%
  select(Trait_trans, moments, siteID, turfID, Temp_yearly_spring, Precip_yearly, Temp_decade, Precip_decade, Temp_annomalies, Precip_annomalies, Temp_level, Precip_level, value, year, n) %>% 
  mutate(value = scale(value)) %>% 
  group_by(Trait_trans, moments, n) %>% 
  nest()

memodel_data_fullcommunity_nottransformed <- moments_clim_long_fullcommunity %>% 
  ungroup() %>%
  select(Trait_trans, moments, siteID, turfID, Temp_yearly_spring, Precip_yearly, Temp_decade, Precip_decade, Temp_annomalies, Precip_annomalies, Temp_level, Precip_level, value, year, n) %>% 
  group_by(Trait_trans, moments, n) %>% 
  nest()

# Without intraspecific variability
memodel_data_without_intra <- moments_clim_long_without_intra %>% 
  ungroup() %>%
  select(Trait_trans, moments, siteID, turfID,Temp_yearly_spring, Precip_yearly, Temp_decade, Precip_decade, Temp_annomalies, Precip_annomalies, Temp_level, Precip_level, value, year, n) %>% 
  mutate(value = scale(value)) %>% 
  group_by(Trait_trans, moments, n) %>% 
  nest()

memodel_data_without_intra_nottransformed <- moments_clim_long_without_intra %>% 
  ungroup() %>%
  select(Trait_trans, moments, siteID, turfID, Temp_yearly_spring, Precip_yearly, Temp_decade, Precip_decade, Temp_annomalies, Precip_annomalies, Temp_level, Precip_level, value, year, n) %>% 
  group_by(Trait_trans, moments, n) %>% 
  nest()


com_data <- community_for_analysis %>%
  group_by(turfID, year) %>% 
  mutate(species_richness = n()) %>% 
  unique() %>% 
  group_by(turfID, year) %>% 
  pivot_wider(names_from = functionalGroup, values_from = cover) %>% 
  mutate(graminoid_cover = sum(graminoid, na.rm = TRUE),
         forb_cover = sum(forb, na.rm = TRUE),
         other_cover = sum(woody, pteridophyte, `NA`, na.rm = TRUE)) %>% 
  left_join(env, by = c("siteID" = "siteID", "year" = "Year")) %>% 
  pivot_longer(cols = c("species_richness", "graminoid_cover", "forb_cover", "other_cover", "total_vascular", "vegetation_height"), names_to = "community_properties", values_to = "value") %>% 
  select(siteID, turfID, Temp_yearly_spring, Precip_yearly, Temp_decade, Precip_decade, Temp_annomalies, Precip_annomalies, Temp_level, Precip_level, year, value, community_properties) %>% 
  group_by(community_properties) %>% 
  unique() %>% 
  mutate(value = scale(value)) %>% 
  nest()

com_data_nottrans <- community_for_analysis %>%
  group_by(turfID, year) %>% 
  mutate(species_richness = n()) %>% 
  unique() %>% 
  group_by(turfID, year) %>% 
  pivot_wider(names_from = functionalGroup, values_from = cover) %>% 
  mutate(graminoid_cover = sum(graminoid, na.rm = TRUE),
         forb_cover = sum(forb, na.rm = TRUE),
         other_cover = sum(woody, pteridophyte, `NA`, na.rm = TRUE)) %>% 
  left_join(env, by = c("siteID" = "siteID", "year" = "Year")) %>% 
  pivot_longer(cols = c("species_richness", "graminoid_cover", "forb_cover", "other_cover", "total_vascular", "vegetation_height"), names_to = "community_properties", values_to = "value") %>% 
  select(siteID, turfID, Temp_yearly_spring, Precip_yearly, Temp_decade, Precip_decade, Temp_annomalies, Precip_annomalies,Temp_level, Precip_level, year, value, community_properties) %>% 
  group_by(community_properties) %>% 
  unique() %>% 
  nest()

#### Functions for models and model outputs ####

model_TDT <- function(df) {
  lmer(value ~ scale(Temp_decade) + scale(Precip_decade) + scale(Temp_annomalies) + scale(Precip_annomalies) + 
         scale(Temp_decade)*scale(Precip_decade) + scale(Temp_annomalies)*scale(Precip_annomalies) + 
         scale(Temp_decade)*scale(Temp_annomalies) + scale(Temp_decade)*scale(Precip_annomalies) + 
         scale(Precip_decade)*scale(Temp_annomalies) + scale(Precip_decade)*scale(Precip_annomalies) +
         + (1|siteID), data = df)
}

output <-function(dat) {
  
  model_output <- dat %>% 
    select(Trait_trans, moments, n, model_output, R_squared) %>% 
    unnest(c(model_output, R_squared)) %>% 
    filter(!term %in% c("(Intercept)", "sd__(Intercept)", "sd__Observation")) %>% 
    select(Trait_trans, moments, term, n, estimate, std.error, statistic, df, p.value, Marginal, Conditional) %>% 
    ungroup() %>% 
    group_by(Trait_trans, moments, term) %>% 
    summarize(effect = mean(estimate),
              R2_marginal = mean(Marginal),
              R2_conditional = mean(Conditional),
              CIlow.fit = effect - sd(estimate),
              CIhigh.fit = effect + sd(estimate),
              std.error = mean(std.error),
              staticstic = mean(statistic),
              df = mean(df),
              p.value = mean(p.value))
  
  return(model_output)
}

output_com <-function(dat) {
  
  model_output <- dat %>% 
    select(community_properties, model_output, R_squared) %>% 
    unnest(c(model_output, R_squared)) %>% 
    filter(!term %in% c("(Intercept)", "sd__(Intercept)", "sd__Observation")) %>% 
    select(community_properties, term, estimate, std.error, statistic, df, p.value, Marginal, Conditional) %>% 
    ungroup() 
  
  return(model_output)
}


#### Running models - traits ####

# Running the mixed effects model

results_TDT <- memodel_data_fullcommunity %>%
  filter(moments %in% c("mean", "skewness")) %>% 
  mutate(model = purrr::map(data, model_TDT))

#Tidying up the model output

tidy_TDT <- results_TDT %>%
  mutate(model_output = purrr::map(model, tidy)) %>%
  mutate(R_squared = purrr::map(model, rsquared))

# Making a dataset with the model output and the test-statistics (R squared), sumarrizing across boootstraps.

output_TDT <- output(tidy_TDT) %>% 
  mutate_if(is.numeric, round, digits = 5)

#write.table(output_TDT, row.names = TRUE, col.names = TRUE, file = "model_output_TDT.csv")


#### Running models - community ####

#Running the mixed effect model

results_TDT_com <- com_data %>%
  mutate(model = purrr::map(data, model_TDT))

#Tidying up the model output

tidy_TDT_com <- results_TDT_com %>%
  mutate(model_output = purrr::map(model, tidy)) %>%
  mutate(R_squared = purrr::map(model, rsquared))


# Making a dataset with the model output and the test-statistics (R squared)

output_TDT_com <- output_com(tidy_TDT_com) %>% 
  mutate_if(is.numeric, round, digits = 5)


#### Simpler mixed effect models on specific traits to make predicted plots ####


model_trait_summary <-function(dat, trait, moment) {
  
  
  # Filter data for model
  dat2 <- dat %>%
    filter(Trait_trans == trait,
           moments == moment,
           n == 75) %>% 
    unnest(data) %>% 
    ungroup() 
  
  # Run model
  model <-   lmer(value ~ scale(Temp_decade) + scale(Precip_decade) + scale(Temp_annomalies) + scale(Precip_annomalies) + 
                    scale(Temp_decade)*scale(Precip_decade) + scale(Temp_annomalies)*scale(Precip_annomalies) + 
                    scale(Temp_decade)*scale(Temp_annomalies) + scale(Temp_decade)*scale(Precip_annomalies) + 
                    scale(Precip_decade)*scale(Temp_annomalies) + scale(Precip_decade)*scale(Precip_annomalies) +
                    + (1|siteID), data = dat2)
  
  return(model)
}

models_trait_predictions_space <-function(model) {

  newdata <- expand.grid(Precip_decade = c(0.8, 1.5, 2.3, 3.5), 
                         Temp_decade = seq(5.5,12, length = 200), 
                         siteID = c("Alrust", "Arhelleren", "Fauske", "Gudmedalen", "Hogsete", "Lavisdalen", "Ovstedalen", "Rambera", "Skjelingahaugen", "Ulvehaugen", "Veskre", "Vikesland"),
                         Precip_annomalies = 0,
                         Temp_annomalies = 0)
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE)
  
  return(newdata)
}

models_trait_predictions_time <-function(model) {
  
  newdata <- expand.grid(Precip_decade = 1.8, 
                         Temp_decade = 9, 
                         siteID = c("Alrust", "Arhelleren", "Fauske", "Gudmedalen", "Hogsete", "Lavisdalen", "Ovstedalen", "Rambera", "Skjelingahaugen", "Ulvehaugen", "Veskre", "Vikesland"),
                         Precip_annomalies = c(-1.5, 0, 1.5, 3),
                         Temp_annomalies = seq(-3, 2, length = 200))
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE)
  
  return(newdata)
}



#### Make datasets with modeled values for different traits for plotting ####

SLA_mean_sum_yc <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "SLA_cm2_g_log", "mean")
SLA_mean_pred_space <- models_trait_predictions_space(SLA_mean_sum_yc)
SLA_mean_pred_time <- models_trait_predictions_time(SLA_mean_sum_yc)

Height_mean_sum_yc <- model_trait_summary_year_clim(memodel_data_fullcommunity_nottransformed, "Plant_Height_mm_log", "mean")
Height_mean_pred_yc <- models_trait_predictions_year_clim(Height_mean_sum_yc)


SLA_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "SLA_cm2_g_log", "mean")
SLA_mean_pred <- models_trait_predictions(SLA_mean_sum)
SLA_skew_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "SLA_cm2_g_log", "skewness")
SLA_skew_pred <- models_trait_predictions(SLA_skew_sum)

SLA_mean_spatial_climate_sum <- model_trait_summary_spatial_climate(memodel_data_fullcommunity_nottransformed, "SLA_cm2_g_log", "mean")
SLA_mean_spatial_pred <- models_trait_predictions_siteID(SLA_mean_spatial_climate_sum)


Height_mean_temporal_sum <- model_trait_summary_temporal(memodel_data_fullcommunity_nottransformed, "Plant_Height_mm_log", "mean")
Height_mean_temporal_pred <- models_trait_predictions_siteID(Height_mean_temporal_sum)

Height_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "Plant_Height_mm_log", "mean")
Height_mean_pred <- models_trait_predictions(Height_mean_sum)



CN_ratio_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "CN_ratio_log", "mean")
CN_ratio_mean_pred <- models_trait_predictions(CN_ratio_mean_sum)
CN_ratio_skew_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "CN_ratio_log", "skewness")
CN_ratio_skew_pred <- models_trait_predictions(CN_ratio_skew_sum)

LA_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "Leaf_Area_cm2_log", "mean")
LA_mean_pred <- models_trait_predictions(LA_mean_sum)
LA_skew_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "Leaf_Area_cm2_log", "skewness")
LA_skew_pred <- models_trait_predictions(LA_skew_sum)


C_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "C_percent", "mean")
C_mean_pred <- models_trait_predictions(C_mean_sum)
C_skew_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "C_percent", "skewness")
C_skew_pred <- models_trait_predictions(C_skew_sum)

N_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "N_percent", "mean")
N_mean_pred <- models_trait_predictions(N_mean_sum)
N_skew_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "N_percent", "skewness")
N_skew_pred <- models_trait_predictions(N_skew_sum)

Height_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "Plant_Height_mm_log", "mean")
Height_mean_pred <- models_trait_predictions(Height_mean_sum)
Height_skew_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "Plant_Height_mm_log", "skewness")
Height_skew_pred <- models_trait_predictions(Height_skew_sum)

Mass_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "Dry_Mass_g_log", "mean")
Mass_mean_pred <- models_trait_predictions(Mass_mean_sum)
Mass_skew_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "Dry_Mass_g_log", "skewness")
Mass_skew_pred <- models_trait_predictions(Mass_skew_sum)

LDMC_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "LDMC", "mean")
LDMC_mean_pred <- models_trait_predictions(LDMC_mean_sum)
LDMC_skew_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "LDMC", "skewness")
LDMC_skew_pred <- models_trait_predictions(LDMC_skew_sum)

Lth_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "Leaf_Thickness_Ave_mm", "mean")
Lth_mean_pred <- models_trait_predictions(Lth_mean_sum)
Lth_skew_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "Leaf_Thickness_Ave_mm", "skewness")
Lth_skew_pred <- models_trait_predictions(Lth_skew_sum)


### Finding the start values for skewness in 2009 to see change in time ###

skewness_2009 <- moments_clim_long_fullcommunity %>% 
  select(siteID, Trait_trans, year, moments, value) %>% 
  filter(moments == "skewness",
         year == "2009") %>% 
  group_by(Trait_trans) %>% 
  summarise(skewness = mean(value),
            CIlow = skewness - sd(value),
            CIhigh = skewness + sd(value))

#### Correlation #### Needs to be updated

# Making data ready for correlation tests

Corr_traits <- sum_moments_climate_fullcommunity %>% 
  ungroup() %>% 
  select(Site, turfID, Trait_trans, mean, Precip_yearly, Temp_yearly_spring, Temp_yearly_prev,  Temp_summer, Precip_yearly_spring) %>% 
  spread(key = Trait_trans, value = mean) %>% 
  select(-Site, -turfID) 

#### Ordination ####

### Make data ready for ordination 

Ord_boot_traits <- sum_moments_climate_fullcommunity %>% 
  ungroup() %>% 
  mutate(uniqueID = paste0(turfID,"_", year, "_", siteID),
         templevel_year = paste0(Temp_level, "_", year)) %>% 
  group_by(uniqueID, Trait_trans) %>% 
  mutate(mean_mean = mean(mean)) %>% 
  filter(!Trait_trans == "Wet_Mass_g_log") %>% 
  select(uniqueID, Site, year, templevel_year, turfID, Trait_trans, Temp_level, Precip_level, mean_mean) %>%
  unique() %>% 
  #gather(Moment, Value, -(turfID:P_cat)) %>% 
  #unite(temp, Trait, Moment) %>% 
  pivot_wider(names_from = Trait_trans, values_from = mean_mean) %>% 
  column_to_rownames("uniqueID") %>% 
  rename("C % "= "C_percent", "C/N" = "CN_ratio_log", "Dry mass" = "Dry_Mass_g_log", "Leaf area" = "Leaf_Area_cm2_log", "Leaf thickness" = "Leaf_Thickness_Ave_mm", "N %" = "N_percent", "Plant height" = "Plant_Height_mm_log", "SLA" = "SLA_cm2_g_log")

### Do ordination
pca_trait <- prcomp(Ord_boot_traits[, -(1:6)], scale = TRUE)

### Get variable
var <- get_pca_var(pca_trait)

### Get results
pca_trait_results <- get_pca_ind(pca_trait)

#### Constrained ordination ####
RDA_temp <- rda(Ord_boot_traits[, -(1:6)]~ Temp_level, scale = TRUE, data = Ord_boot_traits)
RDA_precip <- rda(Ord_boot_traits[, -(1:6)]~ Precip_level, scale = TRUE, data = Ord_boot_traits)
RDA_space_additive <- rda(Ord_boot_traits[, -(1:6)]~ Temp_level+Precip_level, scale = TRUE, data = Ord_boot_traits)

RDA_space <- rda(Ord_boot_traits[, -(1:6)]~ Temp_level*Precip_level, scale = TRUE, data = Ord_boot_traits)
RDA_without_interaction <- rda(Ord_boot_traits[, -(1:6)]~ Temp_level*Precip_level + year, scale = TRUE, data = Ord_boot_traits)
RDA_space_and_time <- rda(Ord_boot_traits[, -(1:6)]~ Temp_level*Precip_level * year, scale = TRUE, data = Ord_boot_traits)
RDA_time <- rda(Ord_boot_traits[, -(1:6)]~ year, scale = TRUE, data = Ord_boot_traits)
RDA_temp_time <- rda(Ord_boot_traits[, -(1:6)]~ Temp_level*year, scale = TRUE, data = Ord_boot_traits)
RDA_precip_time <- rda(Ord_boot_traits[, -(1:6)]~ Precip_level*year, scale = TRUE, data = Ord_boot_traits)


anova(RDA_space, RDA_without_interaction)
anova(RDA_space, RDA_space_and_time)
anova(RDA_without_interaction, RDA_space_and_time)
anova(RDA_space, RDA_space_and_time)

anova(RDA_temp, RDA_space)
anova(RDA_precip, RDA_space)
anova(RDA_space_additive, RDA_space)

anova(RDA_precip, RDA_precip_time)


RsquareAdj(RDA_space_and_time)$adj.r.squared

 