#### Run analysis ####

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
         Precip_deviation_decade = Precip_deviation_decade/1000,
         Precip_century = Precip_century/1000)

## Climate data - making model for mean change in temp and precip ##

temp_model <- lm(Temp_yearly_spring ~ Year + Site, data = env)
precip_model <- lm(Precip_yearly ~ Year + Site, data = env)

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

#### Trait impute ####

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
     filter(siteID %in% c("Hogsete", "Ulvehaugen", "Vikesland", "Dumedalen", "Rambera", "Arhelleren"))
  
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


#### Mixed effect model testing ####

### Making dataset for models ###
 
# With intraspecific variability
memodel_data_fullcommunity <- moments_clim_long_fullcommunity %>% 
  ungroup() %>%
  select(Trait_trans, moments, siteID, turfID, Temp_yearly_spring, Precip_yearly, temp_modeled, precip_modeled, value, year, n) %>% 
  mutate(value = scale(value)) %>% 
  group_by(Trait_trans, moments, n) %>% 
  nest()

memodel_data_fullcommunity_nottransformed <- moments_clim_long_fullcommunity %>% 
  ungroup() %>%
  select(Trait_trans, moments, siteID, turfID, Temp_yearly_spring, Precip_yearly, temp_modeled, precip_modeled, value, year, n) %>% 
  group_by(Trait_trans, moments, n) %>% 
  nest()

# Without intraspecific variability
memodel_data_without_intra <- moments_clim_long_without_intra %>% 
  ungroup() %>%
  select(Trait_trans, moments, siteID, turfID,Temp_yearly_spring, Precip_yearly, temp_modeled, precip_modeled, value, year, n) %>% 
  mutate(value = scale(value)) %>% 
  group_by(Trait_trans, moments, n) %>% 
  nest()

memodel_data_without_intra_nottransformed <- moments_clim_long_without_intra %>% 
  ungroup() %>%
  select(Trait_trans, moments, siteID, turfID, Temp_yearly_spring, Precip_yearly, temp_modeled, precip_modeled, value, year, n) %>% 
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
  select(siteID, turfID, Temp_yearly_spring, Precip_yearly, temp_modeled, precip_modeled, year, value, community_properties) %>% 
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
  select(siteID, turfID, Temp_yearly_spring, Precip_yearly, temp_modeled, precip_modeled, year, value, community_properties) %>% 
  group_by(community_properties) %>% 
  unique() %>% 
  nest()

### Funcitons for different models and model predictions later ###

## 1. model testing if traits shift with climate fluctuations (warm years vs. cold years)

model_time <- function(df) {
  lmer(value ~ scale(Temp_yearly_spring) * scale(Precip_yearly) + (1 | siteID), data = df)
}

## 2. model testing if traits shifts in time using year as the fixed effect

model_time_year <- function(df) {
  lmer(value ~ year + (1 | siteID), data = df)
}

## 2. model testing if traits shift directionally by using the modeled climate data for directional shift in temp and precip in the ten years.

model_time_predicted <- function(df) {
  lmer(value ~scale(temp_modeled) * scale(precip_modeled) + (1 | siteID), data = df)
}

## 3. and 4. model testing if traits shift along the spatial gradient. One will be using the bootstrapped traits with intraspecific variability, and one without the intraspecific variability

model_space <- function(df) {
  lmer(value ~  scale(Temp_yearly_spring) * scale(Precip_yearly) + (1 | year), data = df)
}

## Models 1-4. but with linear models. Results goes in the appendix of the paper.

# model_space_linear<-function(df) {
#   lm(value ~  Temp_yearly_spring * scale(Precip_yearly) +  year, data = df)
# }

# model_time_predicted <- function(df) {
#   lm(value ~scale(temp_modeled) * scale(precip_modeled) + siteID, data = df)
# }

# 
# model_time_linear<-function(df) {
#   lm(value ~ Temp_yearly_spring * scale(Precip_yearly) +  siteID, data = df)
# }


## Function for predictions from the above models
predict_without_random<-function(model) {
  predict(object = model, re.form=NA)
}

predict_with_random<-function(model) {
  predict(object = model, re.form=NULL)
}


#### Running models (time consuming), 2 steps ####

# 1) Running the mixed effects model
# 2) Tidying the model, giving the model output in a nice format. Calculating pseudo R squared values based on the method in paper Nakagawa et al 2017.


## Model 1: Trait shifts with time/climate fluctuations ##
# Step 1) Model
mem_results_time_mixed <- memodel_data_fullcommunity %>%
  filter(moments %in% c("mean", "skewness")) %>% 
  mutate(model = purrr::map(data, model_time))

#Step 2) Tidying model output, getting predictions and R2
tidy_time_model_predicted_mixed <- mem_results_time_mixed %>%
  mutate(model_output = purrr::map(model, tidy)) %>%
  mutate(R_squared = purrr::map(model, rsquared))

## Model 2: Trait shifts with time - year ##
# Step 1) Model
mem_results_year_mixed <- memodel_data_fullcommunity %>%
  filter(moments %in% c("mean", "skewness")) %>% 
  mutate(model = purrr::map(data, model_time_year))

#Step 2) Tidying model output, getting predictions and R2
tidy_year_model_predicted_mixed <- mem_results_year_mixed %>%
  mutate(model_output = purrr::map(model, tidy)) %>%
  mutate(R_squared = purrr::map(model, rsquared))

## Model 1 without transforming the data: Trait shifts with time/climate fluctuations
# mem_results_time_mixed_nottrans <- memodel_data_fullcommunity_nottransformed %>%
#   filter(moments %in% c("mean", "skewness")) %>% 
#   mutate(model = purrr::map(data, model_time))
# 
# tidy_time_model_predicted_mixed_nottrans <- mem_results_time_mixed_nottrans %>%
#   mutate(model_output = purrr::map(model, tidy)) %>%
#   mutate(R_squared = purrr::map(model, rsquared))


## Model 1: Community shifts with time/climate fluctuations -Community ##
# Step 1) Model
mem_results_com_time_mixed <- com_data %>%
  mutate(model = purrr::map(data, model_time))

#Step 2) Tidying model output, getting predictions and R2
tidy_com_time_model_predicted_mixed <- mem_results_com_time_mixed %>%
  mutate(model_output = purrr::map(model, tidy)) %>%
  mutate(R_squared = purrr::map(model, rsquared)) 

## Model 2: Trait shifts with time - year - Community ##
# Step 1) Model
mem_results_com_time_year_mixed <- com_data %>%
  mutate(model = purrr::map(data, model_time_year))

#Step 2) Tidying model output, getting predictions and R2
tidy_com_time_year_model_predicted_mixed <- mem_results_com_time_year_mixed %>%
  mutate(model_output = purrr::map(model, tidy)) %>%
  mutate(R_squared = purrr::map(model, rsquared)) 

## Model 1 without transforming the data: Community shifts with time/climate flucuations
# mem_results_com_time_mixed_nottrans <- com_data_nottrans %>%
#   mutate(model = purrr::map(data, model_time))
# 
# tidy_com_time_model_predicted_mixed_nottrans <- mem_results_com_time_mixed_nottrans %>%
#   mutate(model_output = purrr::map(model, tidy)) %>%
#   mutate(R_squared = purrr::map(model, rsquared)) 


## Model 2: Trait shifts with time, directional shifts ##
# Step 1) Model
mem_results_time_directional_mixed <- memodel_data_fullcommunity %>%
  filter(moments %in% c("mean", "skewness")) %>% 
  mutate(model = purrr::map(data, model_time_predicted))

#Step 2) Tidying model output, getting predictions and R2
tidy_time_directional_model_predicted_mixed <- mem_results_time_directional_mixed %>%
  mutate(model_output = purrr::map(model, tidy)) %>%
  mutate(R_squared = purrr::map(model, rsquared))



## Model 3: Trait shifts in space, with intraspecific variability ##
# Step 1) Model
mem_results_space <- memodel_data_fullcommunity %>%
  filter(moments %in% c("mean", "skewness")) %>% 
  mutate(model = purrr::map(data, model_space))

#Step 2) Tidying model output, getting predictions and R2
tidy_space_model_predicted_mixed <- mem_results_space %>%
  mutate(model_output = purrr::map(model, tidy)) %>%
  mutate(R_squared = purrr::map(model, rsquared))

 # mem_results_space_nottrans <- memodel_data_fullcommunity_nottransformed %>%
 #   filter(moments %in% c("mean", "skewness")) %>% 
 #   mutate(model = purrr::map(data, model_space))
 # 
 # tidy_space_model_predicted_mixed_nottrans <- mem_results_space_nottrans %>%
 #   mutate(model_output = purrr::map(model, tidy)) %>%
 #   mutate(R_squared = purrr::map(model, rsquared))
 

## Model 4: Trait shifts in space, without intraspecific variability ##
# Step 1) Model
mem_results_space_wi <- memodel_data_without_intra%>%
   filter(moments %in% c("mean", "skewness")) %>% 
   mutate(model = purrr::map(data, model_space))
 
#Step 2) Tidying model output, getting predictions and R2
tidy_space_model_predicted_mixed_wi <- mem_results_space_wi %>%
   mutate(model_output = purrr::map(model, tidy)) %>%
   mutate(R_squared = purrr::map(model, rsquared))
 
# mem_results_space_nottrans_wi <- memodel_data_without_intra_nottransformed %>%
#     filter(moments %in% c("mean", "skewness")) %>% 
#     mutate(model = purrr::map(data, model_space))
#   
# tidy_space_model_predicted_mixed_nottrans_wi <- mem_results_space_nottrans_wi %>%
#     mutate(model_output = purrr::map(model, tidy)) %>%
#     mutate(R_squared = purrr::map(model, rsquared))


## Model space community: Community shifts in space ##
# Step 1) Model
mem_results_com_space <- com_data %>%
  mutate(model = purrr::map(data, model_space))

#Step 2) Tidying model output, getting predictions and R2
tidy_com_space_model_predicted_mixed <- mem_results_com_space %>%
  mutate(model_output = purrr::map(model, tidy)) %>%
  mutate(R_squared = purrr::map(model, rsquared))

# mem_results_com_space_nottrans <- com_data_nottrans %>%
#   mutate(model = purrr::map(data, model_space))
# 
# tidy_com_space_model_predicted_mixed_nottrans <- mem_results_com_space_nottrans %>%
#   mutate(model_output = purrr::map(model, tidy)) %>%
#   mutate(R_squared = purrr::map(model, rsquared))

# predicted_values_space_mixed <- tidy_space_model_predicted %>%
#   ungroup() %>%
#   select(Trait_trans, moments, data, predicted) %>%
#   unnest(c(data, predicted)) %>%
#   rename(modeled = predicted, measured = value)


 
# mem_results_time_mixed_notax <- memodel_data_fullcommunity_notax %>%
#   filter(moments %in% c("mean", "skewness")) %>% 
#   mutate(model = purrr::map(data, model_time))
# 
# tidy_time_model_predicted_mixed_notax <- mem_results_time_mixed_notax %>%
#   mutate(model_output = purrr::map(model, tidy)) %>%
#   mutate(predicted = purrr::map(model, predict_with_random)) %>% 
#   mutate(R_squared = purrr::map(model, rsquared))

# predicted_values_time <- tidy_time_model_predicted %>%
#   ungroup() %>%
#   select(Trait_trans, moments, data, predicted) %>%
#   unnest(c(data, predicted)) %>%
#   rename(modeled = predicted, measured = value)


## Space linear ##
# mem_results_space_linear <- memodel_data_fullcommunity %>%
#   filter(moments %in% c("mean", "skewness"),
#          n == 4) %>% 
#   mutate(model = purrr::map(data, model_space_linear))
# 
# tidy_space_model_predicted_linear <- mem_results_space_linear %>%
#   mutate(model_output = purrr::map(model, tidy)) %>% 
#   mutate(predicted = purrr::map(model, predict)) %>% 
#   mutate(R_squared = purrr::map(model, rsquared))
# 
# predicted_values_space_ <- tidy_space_model_predicted %>%
#    ungroup() %>%
#    select(Trait_trans, moments, data, predicted) %>%
#    unnest(c(data, predicted)) %>%
#    rename(modeled = predicted, measured = value)


# ## Without intraspecific variability ##
# mem_results_space_without_intra <- memodel_data_without_intra %>%
#   mutate(model = map(data, model_space))
# 
# tidy_space_model_predicted_without_intra <- mem_results_space_without_intra %>%
#   mutate(model_output = map(model, tidy)) %>%
#   #mutate(predicted = map(model, predict_with_random)) %>% 
#   mutate(R_squared = map(model, rsquared))
# 
# predicted_values_space_without_intra <- tidy_space_model_predicted_without_intra %>%
#   ungroup() %>%
#   select(Trait_trans, moments, data, predicted) %>%
#   unnest(c(data, predicted)) %>%
#   rename(modeled = predicted, measured = value)


## Time linear ##
# mem_results_time_linear <- memodel_data_fullcommunity %>%
#   filter(moments %in% c("mean", "skewness"),
#          n == 4) %>% 
#   mutate(model = purrr::map(data, model_time_linear))
# 
# tidy_time_model_predicted_linear <- mem_results_time_linear %>%
#   mutate(model_output = purrr::map(model, tidy)) %>% 
#   mutate(predicted = purrr::map(model, predict)) %>% 
#   mutate(R_squared = purrr::map(model, rsquared))

# Making a function for AIC tests
# 
#  AIC_models <-function(dat, model_type) {
#    
#    dat2 <- dat %>% 
#      mutate(AIC = map(model, AIC)) %>% 
#      select(Trait_trans, moments, n, AIC) %>% 
#      unnest(c(AIC)) %>% 
#      ungroup() %>% 
#      group_by(Trait_trans, moments) %>% 
#      mutate(AIC = mean(AIC)) %>% 
#      select(-n) %>% 
#      unique()
#    
#    return(dat2)
#  }
#  
#  # Get AIC for all models and compare which is better
#  
#  AIC_lastyeartemp <- AIC_models(tidy_space_model_predicted_lastyeartemp, "Last year temp") %>% rename(Lastyeartemp_AIC = AIC)
#  AIC_springtemp <- AIC_models(tidy_space_model_predicted_springtemp, "Spring temp") %>% rename(Springtemp_AIC = AIC)
#  AIC_alltemp <- AIC_models(tidy_space_model_predicted, "full temp") %>% rename(full_AIC = AIC)
#  
#  AIC_all_models <- AIC_lastyeartemp %>%
#    left_join(AIC_springtemp, by = c("Trait_trans" = "Trait_trans", "moments" = "moments")) %>% 
#    left_join(AIC_alltemp, by = c("Trait_trans" = "Trait_trans", "moments" = "moments"))
  
#write.table(x = AIC_all_models, file = "AIC_log.csv")

# Making a dataset with the model output and the test-statistics (R squared), and summarizing them.
model_output_mixed <-function(dat) {
  
  model_output <- dat %>% 
    select(Trait_trans, moments, n, model_output, R_squared) %>% 
    unnest(c(model_output, R_squared)) %>% 
    filter(term %in% c("scale(Precip_yearly)", "scale(Temp_yearly_spring)", "scale(Temp_yearly_spring):scale(Precip_yearly)")) %>% 
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

model_output_mixed_year <-function(dat) {
  
  model_output <- dat %>% 
    select(Trait_trans, moments, n, model_output, R_squared) %>% 
    unnest(c(model_output, R_squared)) %>% 
    filter(term %in% c("(Intercept)", "year")) %>% 
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

model_output_mixed_predicted_climate <-function(dat) {
  
  model_output <- dat %>% 
    select(Trait_trans, moments, n, model_output, R_squared) %>% 
    unnest(c(model_output, R_squared)) %>% 
    filter(term %in% c( "scale(temp_modeled)", "scale(temp_modeled):scale(precip_modeled)")) %>% 
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

model_output_com_mixed <-function(dat) {
  
  model_output <- dat %>% 
    select(community_properties, model_output, R_squared) %>% 
    unnest(c(model_output, R_squared)) %>% 
    filter(term %in% c("scale(Precip_yearly)", "scale(Temp_yearly_spring)", "scale(Temp_yearly_spring):scale(Precip_yearly)")) %>% 
    select(community_properties, term, estimate, std.error, statistic, df, p.value, Marginal, Conditional) %>% 
    ungroup() 
  
  return(model_output)
}

model_output_com_year_mixed <-function(dat) {
  
  model_output <- dat %>% 
    select(community_properties, model_output, R_squared) %>% 
    unnest(c(model_output, R_squared)) %>% 
    filter(term %in% c("year")) %>% 
    select(community_properties, term, estimate, std.error, statistic, df, p.value, Marginal, Conditional) %>% 
    ungroup() 
  
  return(model_output)
}



model_output_linear <-function(dat) {
  
  model_output <- dat %>% 
    select(Trait_trans, moments, n, model_output, R_squared) %>% 
    #filter(Trait_trans %in% c("CN_ratio_log", "SLA_cm2_g_log") & moments %in% c("mean", "variance")) %>% 
    #filter(!Trait_trans == "CN_ratio_log" | moments == "variance") %>% 
    unnest(c(model_output, R_squared)) %>% 
    filter(term %in% c("scale(Precip_yearly)", "scale(Temp_yearly_spring)", "scale(Temp_yearly_spring):scale(Precip_yearly)")) %>% 
    select(Trait_trans, moments, term, n, estimate, std.error, statistic, p.value, R.squared) %>% 
    ungroup() %>% 
    group_by(Trait_trans, moments, term) %>% 
    summarize(effect = mean(estimate),
              R2 = mean(R.squared),
              std.error = mean(std.error),
              staticstic = mean(statistic),
              p.value = mean(p.value))
  return(model_output)
}


model_output_time_mixed <- model_output_mixed(tidy_time_model_predicted_mixed) %>% 
  mutate_if(is.numeric, round, digits = 5)

model_output_time_year_mixed <- model_output_mixed_year(tidy_year_model_predicted_mixed) %>% 
  mutate_if(is.numeric, round, digits = 6)

model_output_time_directional_mixed <- model_output_mixed_predicted_climate(tidy_time_directional_model_predicted_mixed)%>% 
  mutate_if(is.numeric, round, digits = 3)
#model_output_time_mixed_nottrans <- model_output_mixed(tidy_time_model_predicted_mixed_nottrans) %>% 
#  mutate_if(is.numeric, round, digits = 3)
model_output_time_mixed_wi <- model_output_mixed(tidy_time_model_predicted_mixed_wi)%>% 
  mutate_if(is.numeric, round, digits = 3)
#model_output_time_linear <- model_output_linear(tidy_time_model_predicted_linear)
model_output_com_time_mixed <- model_output_com_mixed(tidy_com_time_model_predicted_mixed) %>% 
  mutate_if(is.numeric, round, digits = 3)
model_output_com_time_year_mixed <- model_output_com_year_mixed(tidy_com_time_year_model_predicted_mixed) %>% 
  mutate_if(is.numeric, round, digits = 3)
# model_output_com_time_mixed_nottrans <- model_output_com_mixed(tidy_com_time_model_predicted_mixed_nottrans) %>% 
#   mutate_if(is.numeric, round, digits = 3)

model_output_space_mixed <- model_output_mixed(tidy_space_model_predicted_mixed) %>% 
  mutate_if(is.numeric, round, digits = 3)
# model_output_space_mixed_nottrans <- model_output_mixed(tidy_space_model_predicted_mixed_nottrans) %>% 
#   mutate_if(is.numeric, round, digits = 3)
model_output_space_mixed_wi <- model_output_mixed(tidy_space_model_predicted_mixed_wi) %>% 
  mutate_if(is.numeric, round, digits = 3)
# model_output_space_mixed_nottrans_wi <- model_output_mixed(tidy_space_model_predicted_mixed_nottrans_wi) %>% 
#   mutate_if(is.numeric, round, digits = 3)
#model_output_space_mixed_notax <- model_output_mixed(tidy_space_model_predicted_mixed_notax)
#model_output_space_linear <- model_output_linear(tidy_space_model_predicted_linear)
model_output_com_space_mixed <- model_output_com_mixed(tidy_com_space_model_predicted_mixed)%>% 
  mutate_if(is.numeric, round, digits = 3)
# model_output_com_space_mixed_nottrans <- model_output_com_mixed(tidy_com_space_model_predicted_mixed_nottrans) %>% 
#   mutate_if(is.numeric, round, digits = 3)


write.table(model_output_time_mixed_nottrans, row.names = TRUE, col.names = TRUE, file = "model_output_time.csv")
write.table(model_output_space_mixed_nottrans, row.names = TRUE, col.names = TRUE, file = "model_output_space.csv")
write.table(model_output_space_mixed_nottrans_wi, row.names = TRUE, col.names = TRUE, file = "model_output_space_withoutITV.csv")

write.table(model_output_com_time_mixed_nottrans, row.names = TRUE, col.names = TRUE, file = "model_output_com_time.csv")
write.table(model_output_com_space_mixed_nottrans, row.names = TRUE, col.names = TRUE, file = "model_output_com_space.csv")

#### Simpler mixed effect models on specific traits to make predicted plots ####

### Making functional for simpler models on single traits 

model_trait_summary <-function(dat, trait, moment) {
  
  
  # Filter data for model
  dat2 <- dat %>%
    filter(Trait_trans == trait,
           moments == moment,
           n == 75) %>% 
    unnest(data) %>% 
    ungroup() 
  
  # Run model
  model <- lmer(value ~ Temp_yearly_spring * Precip_yearly + (1 | year), data = dat2)
  
  return(model)
}

models_trait_predictions <-function(model) {

  newdata <- expand.grid(Precip_yearly=c(0.6, 1.5, 2.3, 3.5), Temp_yearly_spring=seq(3,12, length=500), year = c(2009, 2011, 2012, 2013, 2015, 2016, 2017, 2019))
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE)
  
  return(newdata)
}


### Makgin function for simpler models on single traits with the modeled climate change in the ten year period ###

model_trait_summary_spatial_climate <-function(dat, trait, moment) {
  
  
  # Filter data for model
  dat2 <- dat %>%
    filter(Trait_trans == trait,
           moments == moment,
           n == 75) %>% 
    unnest(data) %>% 
    ungroup() 
  
  # Run model
  model <- lmer(value ~ Temp_yearly_spring * Precip_yearly + (1 | siteID), data = dat2)
  
  return(model)
}

model_trait_summary_temporal <-function(dat, trait, moment) {
  
  
  # Filter data for model
  dat2 <- dat %>%
    filter(Trait_trans == trait,
           moments == moment,
           n == 75) %>% 
    unnest(data) %>% 
    ungroup() 
  
  # Run model
  model <- lmer(value ~ year + (1 | siteID), data = dat2)
  
  return(model)
}

models_trait_predictions_siteID <-function(model) {
  
  newdata <- expand.grid( year = c(2009, 2011, 2012, 2013, 2015, 2016, 2017, 2019), siteID = c("Alrust", "Arhelleren", "Fauske", "Gudmedalen", "Hogsete", "Lavisdalen", "Ovstedalen", "Rambera", "Skjelingahaugen", "Ulvehaugen", "Veskre", "Vikesland"))
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE)
  
  return(newdata)
}


# models_trait_predictions_for_heatmap <-function(model, trait, moment) {
#   
#   newdata <- expand.grid(Precip_yearly=c(0.6, 1.5, 2.3, 3.5), Temp_yearly_spring=c(5.5, 7.5, 10.5), year = c(2009, 2011, 2012, 2013, 2015, 2016, 2017, 2019))
#   
#   newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE)
#   
#   newdata <- newdata %>% 
#   mutate(moment = moment,
#          trait = trait)
#   
#   return(newdata)
# }

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


### Making predictions for modeling using the modeled climate change ##
SLA_mean_direction_climate_sum <- model_trait_summary_directional_climate(memodel_data_fullcommunity_nottransformed, "SLA_cm2_g_log", "mean")
SLA_mean_directional_climate_pred <- models_trait_predictions_directional_climate(SLA_mean_direction_climate_sum)

Height_mean_direction_climate_sum <- model_trait_summary_directional_climate(memodel_data_fullcommunity_nottransformed, "Plant_Height_mm_log", "mean")
Height_mean_direction_climate_pred <- models_trait_predictions_directional_climate(Height_mean_direction_climate_sum)

### Finding the start values for skewness in 2009 to see change in time ###

moments_clim_long_fullcommunity %>% 
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

 RDA <- rda(Ord_boot_traits[, -(1:6)]~ year, scale = TRUE, data = Ord_boot_traits)
 
 autoplot(RDA, arrows = TRUE) +
   theme_bw()
 
 autoplot(RDA, arrows = TRUE, data = Ord_boot_traits) +
   scale_x_continuous(expand = c(0.22, 0)) +
   geom_point(data = RDA, aes(PC1, PC2), size=2) +
   geom_abline(intercept = 0,slope = 0,linetype="dashed", size=0.8) +
   geom_vline(aes(xintercept=0), linetype="dashed", size=0.8) + labs(x = "Axis 1", y="Axis 2") + 
   theme_bw()
 
 RDA_fort <- fortify(RDA)
 
 ggplot(RDA_fort, aes(x = PC1, y = PC2)) +
   geom_point(show.legend = FALSE) +
   scale_size(range = 2) +
   coord_equal() 
 
  
 plot(RDA)
 screeplot(RDA)
 
 coef(RDA)
 
 RsquareAdj(RDA)$adj.r.squared