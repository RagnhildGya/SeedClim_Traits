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

set.seed(47)

#### Making data ready for traitstrap and merging ####

## Community ##

community <- community %>% 
  filter(!year %in% c("2010", "2016"))

community_forb <- community %>% 
  filter(functionalGroup %in% c("forb", "woody", "pteridophyte"))

community_graminoid <- community %>% 
  filter(functionalGroup == "graminoid")

community_alpine <- community %>% 
  filter(siteCode %in% c("alp1", "alp2", "alp3", "alp4"))

community_subalpine <- community %>% 
  filter(siteCode %in% c("int1", "int2", "int3", "int4"))

community_boreal <- community %>% 
  filter(siteCode %in% c("low1", "low2", "low3", "low4"))

## Trait data ##

traitdata_2 <- traitdata_1 %>% 
  mutate(blockID = "",
         turfID = "") 

trait_forb <- traitdata_2 %>% 
  filter(functionalGroup %in% c("forb", "woody", "pteridophyte"))

trait_graminoid <- traitdata_2 %>% 
  filter(functionalGroup == "graminoid")

# env <- env %>% 
#   select(-Temp_se, -Precip_se)

## Make a drake plan DOES NOT WORK YET ##

# ComYearList <- drake_plan(
#   
#   # make a list with all years data
# YearList = list(com2009 = community2009,
#                 com2011 = community2011,
#                 com2012 = community2012,
#                 com2013 = community2013,
#                 com2015 = community2015,
#                 com2016 = community2016,
#                 com2017 = community2017) %>% 
# map(Trait_impute_per_year(trait_dat = traitdata_2, com_dat = .)),
# 
# ) 

#### Trait impute ####



 Trait_impute_per_year <- function(com_dat, trait_dat){
   
   SeedClim_traits <- trait_impute(comm = com_dat,
                                   traits = trait_dat, 
                                   scale_hierarchy = c("Site", "blockID", "turfID"),
                                   global = FALSE,
                                   taxon_col = c("Full_name", "Genus", "Family"),
                                   trait_col = "Trait_trans",
                                   value_col = "Value",
                                   other_col = "year",
                                   abundance_col = "cover")
   
   return(SeedClim_traits)
 }
 
Imputed_traits_fullcommunity <- Trait_impute_per_year(com_dat = community, trait_dat = traitdata_2) %>% 
   group_by(year, Site, blockID, turfID, Trait_trans)

Imputed_traits_forbs <- Trait_impute_per_year(com_dat = community_forb, trait_dat = trait_forb) %>% 
  group_by(year, Site, blockID, turfID, Trait_trans)

Imputed_traits_graminoids <- Trait_impute_per_year(com_dat = community_graminoid, trait_dat = trait_graminoid) %>% 
  group_by(year, Site, blockID, turfID, Trait_trans)

Imputed_traits_alpine <- Trait_impute_per_year(com_dat = community_alpine, trait_dat = traitdata_2) %>% 
  group_by(year, Site, blockID, turfID, Trait_trans)

Imputed_traits_subalpine <- Trait_impute_per_year(com_dat = community_subalpine, trait_dat = traitdata_2) %>% 
  group_by(year, Site, blockID, turfID, Trait_trans)

Imputed_traits_boreal <- Trait_impute_per_year(com_dat = community_boreal, trait_dat = traitdata_2) %>% 
  group_by(year, Site, blockID, turfID, Trait_trans)


# SeedClim_traits_allYears <- trait_impute(comm = community,
#              traits = traitdata_2, 
#              scale_hierarchy = c("Site", "blockID", "turfID"),
#              global = FALSE,
#              taxon_col = c("Full_name", "Genus", "Family"),
#              trait_col = "Trait_trans",
#              value_col = "Value",
#              other_col = "year",
#              abundance_col = "cover") %>% 
#   group_by(year, Site, blockID, turfID, Trait_trans)



#### Bootstraping community weighted means and making summarised moments of the distributions ####

Moments_fullcommunity <- trait_np_bootstrap(imputed_traits = Imputed_traits_fullcommunity)
Moments_forbs <- trait_np_bootstrap(imputed_traits = Imputed_traits_forbs)
Moments_graminoids <- trait_np_bootstrap(imputed_traits = Imputed_traits_graminoids)
Moments_alpine <- trait_np_bootstrap(imputed_traits = Imputed_traits_alpine)
Moments_subalpine <- trait_np_bootstrap(imputed_traits = Imputed_traits_subalpine)
Moments_boreal <- trait_np_bootstrap(imputed_traits = Imputed_traits_boreal)

sum_moments_fullcommunity <- trait_summarise_boot_moments(Moments_fullcommunity)
sum_moments_forbs <- trait_summarise_boot_moments(Moments_forbs)
sum_moments_graminoids <- trait_summarise_boot_moments(Moments_graminoids)
sum_moments_alpine <- trait_summarise_boot_moments(Moments_alpine)
sum_moments_subalpine <- trait_summarise_boot_moments(Moments_subalpine)
sum_moments_boreal <- trait_summarise_boot_moments(Moments_boreal)


#### Adding climate info & pivoting longer ####

sum_moments_climate_fullcommunity = bind_rows(
  sum_moments_fullcommunity %>% 
     left_join(env, by = c("Site" = "Site", "year" = "Year")))

sum_moments_climate_forbs = bind_rows(
  sum_moments_forbs %>% 
    left_join(env, by = c("Site" = "Site", "year" = "Year")))

sum_moments_climate_graminoids = bind_rows(
  sum_moments_graminoids %>% 
    left_join(env, by = c("Site" = "Site", "year" = "Year")))

sum_moments_climate_alpine = bind_rows(
  sum_moments_alpine %>% 
    left_join(env, by = c("Site" = "Site", "year" = "Year")))

sum_moments_climate_subalpine = bind_rows(
  sum_moments_subalpine %>% 
    left_join(env, by = c("Site" = "Site", "year" = "Year")))

sum_moments_climate_boreal = bind_rows(
  sum_moments_boreal %>% 
    left_join(env, by = c("Site" = "Site", "year" = "Year")))



moments_clim_long_fullcommunity <- Moments_fullcommunity %>% 
  pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value") %>% 
  left_join(env, by = c("Site" = "Site", "year" = "Year")) %>% 
  mutate(year = as.factor(year))

moments_clim_long_forbs <- Moments_forbs %>% 
  pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value") %>% 
  left_join(env, by = c("Site" = "Site", "year" = "Year")) %>% 
  mutate(year = as.factor(year))

moments_clim_long_graminoids <- Moments_graminoids %>% 
  pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value") %>% 
  left_join(env, by = c("Site" = "Site", "year" = "Year")) %>% 
  mutate(year = as.factor(year))

moments_clim_long_alpine <- Moments_alpine %>% 
  pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value") %>% 
  left_join(env, by = c("Site" = "Site", "year" = "Year")) %>% 
  mutate(year = as.factor(year))

moments_clim_long_subalpine <- Moments_subalpine %>% 
  pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value") %>% 
  left_join(env, by = c("Site" = "Site", "year" = "Year")) %>% 
  mutate(year = as.factor(year))

moments_clim_long_boreal <- Moments_boreal %>% 
  pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value") %>% 
  left_join(env, by = c("Site" = "Site", "year" = "Year")) %>% 
  mutate(year = as.factor(year))

#### Making data set with with summary information about moments in the data set ####




#### Mixed effect model testing ####

# mixed_model_temp_precip<-function(df) {
#   lmer(value ~ Temp * scale(Precip) +  (1 | Site), data = df)
# }
# 
# mixed_model_clim_year<-function(df) {
#   lmer(value ~ year + (1 | Site), data = df)
# }
# 
# mixed_model_clim_year<-function(df) {
#   lmer(value ~ year + (1 | Site), data = df)
# }
# 
# predict_without_random<-function(model) {
#   predict(object = model, re.form=NA)
# }
# 
# predict_with_random<-function(model) {
#   predict(object = model, re.form=NULL)
# }
# Making  dataset ready for model 

# memodel_data <-function(dat) {
#   dat2 <- dat %>% 
#     select(Trait_trans, moments, Site, blockID, turfID, Temp, Precip, value) %>% 
#     group_by(Trait_trans, moments, turfID) %>% 
#     mutate(n = 1:n()) %>% 
#     ungroup() %>%
#     select(-turfID) %>% 
#     group_by(Trait_trans, moments, n) %>% 
#     nest()
#   return(dat2)
# }
# 
# me_year_model_data <-function(dat) {
#   dat2 <- dat %>% 
#     ungroup() %>% 
#     select(Trait_trans, moments, Site, turfID, n, Temp, Precip, value, year) %>% 
#     # group_by(Trait_trans, moments, turfID) %>% 
#     # mutate(n = 1:n()) %>% 
#     # select(-turfID) %>% 
#     group_by(Trait_trans, moments, n) %>% 
#     nest()
#   return(dat2)
# }

### Making dataset for models ###
memodel_data_fullcommunity <- moments_clim_long_fullcommunity %>% 
  #filter(Trait_trans %in% c("CN_ratio_log", "Leaf_Area_cm2_log", "Plant_Height_mm_log", "SLA_cm2_g_log")) %>% 
  ungroup() %>%
  select(Trait_trans, moments, Site, turfID, Temp_yearly_prev, Temp_yearly_spring, Precip_yearly, Temp_deviation_decade, Precip_deviation_decade, value, year, n) %>% 
  group_by(Trait_trans, moments, n) %>% 
  nest()

memodel_data_forbs <- moments_clim_long_forbs %>% 
  #filter(Trait_trans %in% c("CN_ratio_log", "Leaf_Area_cm2_log", "Plant_Height_mm_log", "SLA_cm2_g_log")) %>% 
  ungroup() %>%
  select(Trait_trans, moments, Site, turfID, Temp_yearly_prev, Temp_yearly_spring,  Precip_yearly, Temp_deviation_decade, Precip_deviation_decade, value, year, n) %>% 
  group_by(Trait_trans, moments, n) %>% 
  nest()

memodel_data_graminoids <- moments_clim_long_graminoids %>% 
  #filter(Trait_trans %in% c("CN_ratio_log", "Leaf_Area_cm2_log", "Plant_Height_mm_log", "SLA_cm2_g_log")) %>% 
  ungroup() %>%
  select(Trait_trans, moments, Site, turfID, Temp_yearly_prev, Temp_yearly_spring,  Precip_yearly, Temp_deviation_decade, Precip_deviation_decade, value, year, n) %>% 
  group_by(Trait_trans, moments, n) %>% 
  nest()

memodel_data_alpine <- moments_clim_long_alpine %>% 
  #filter(Trait_trans %in% c("CN_ratio_log", "Leaf_Area_cm2_log", "Plant_Height_mm_log", "SLA_cm2_g_log")) %>% 
  ungroup() %>%
  select(Trait_trans, moments, Site, turfID, Temp_yearly_prev, Temp_yearly_spring,  Precip_yearly, Temp_deviation_decade, Precip_deviation_decade, value, year, n) %>% 
  group_by(Trait_trans, moments, n) %>% 
  nest()

memodel_data_subalpine <- moments_clim_long_subalpine %>% 
  #filter(Trait_trans %in% c("CN_ratio_log", "Leaf_Area_cm2_log", "Plant_Height_mm_log", "SLA_cm2_g_log")) %>% 
  ungroup() %>%
  select(Trait_trans, moments, Site, turfID, Temp_yearly_prev, Temp_yearly_spring,  Precip_yearly, Temp_deviation_decade, Precip_deviation_decade, value, year, n) %>% 
  group_by(Trait_trans, moments, n) %>% 
  nest()

memodel_data_boreal <- moments_clim_long_boreal %>% 
  #filter(Trait_trans %in% c("CN_ratio_log", "Leaf_Area_cm2_log", "Plant_Height_mm_log", "SLA_cm2_g_log")) %>% 
  ungroup() %>%
  select(Trait_trans, moments, Site, turfID, Temp_yearly_prev, Temp_yearly_spring, Precip_yearly, Temp_deviation_decade, Precip_deviation_decade, value, year, n) %>% 
  group_by(Trait_trans, moments, n) %>% 
  nest()

### Funcitons for different models and model predictions later ###


model_time<-function(df) {
  lmer(value ~ Temp_yearly_prev * scale(Precip_yearly) + Temp_yearly_spring * scale(Precip_yearly) + (1 | Site), data = df)
}

model_space<-function(df) {
  lmer(value ~ Temp_yearly_prev * scale(Precip_yearly) + Temp_yearly_spring * scale(Precip_yearly) + (1 | year), data = df)
}

model_space_1<-function(df) {
  lmer(value ~ Temp_yearly_prev * scale(Precip_yearly) + (1 | year), data = df)
}

model_space_2 <- function(df) {
  lmer(value ~ Temp_yearly_spring * scale(Precip_yearly) + (1 | year), data = df)
}

model_time_anomalies<-function(df) {
  lmer(value ~ Temp_deviation_decade * scale(Precip_deviation_decade) + (1 | Site), data = df)
}


predict_without_random<-function(model) {
  predict(object = model, re.form=NA)
}

predict_with_random<-function(model) {
  predict(object = model, re.form=NULL)
}


#### Running models (time consuming), 3 steps ####

# 1) Testing with mixed effect model
# 2) Tidying the model, giving the model output in a nice format. Making predicted values for each of the trait:moment combination along the climatic gradients. Calculating pseudo R squared values based on the method in paper Nakagawa et al 2017.
# 3) Making a dataset with only the predicted values. Unnesting the list of the predicted values.

## Space ##
mem_results_space <- memodel_data_fullcommunity %>%
  mutate(model = map(data, model_space))

tidy_space_model_predicted <- mem_results_space %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_space <- tidy_space_model_predicted %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)

## Space model 1 ##

mem_results_space_lastyeartemp <- memodel_data_fullcommunity %>%
  mutate(model = map(data, model_space_1))

tidy_space_model_predicted_lastyeartemp <- mem_results_space_lastyeartemp %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

## Space model 2 ##

mem_results_space_springtemp <- memodel_data_fullcommunity %>%
  mutate(model = map(data, model_space_2))

tidy_space_model_predicted_springtemp <- mem_results_space_springtemp %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))


## Time - Full community ##
mem_results_time <- memodel_data_fullcommunity %>%
  mutate(model = map(data, model_time))

tidy_time_model_predicted <- mem_results_time %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_time <- tidy_time_model_predicted %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)

## Time - Forbs ##
mem_results_time_forbs <- memodel_data_forbs %>%
  mutate(model = map(data, model_time))

tidy_time_model_predicted_forbs <- mem_results_time_forbs %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_time_forbs <- tidy_time_model_predicted_forbs %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)

## Time - Graminoids ##
mem_results_time_graminoids <- memodel_data_graminoids %>%
  mutate(model = map(data, model_time))

tidy_time_model_predicted_graminoids <- mem_results_time_graminoids %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_time_graminoids <- tidy_time_model_predicted_graminoids %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)

## Time - alpine ##
mem_results_time_alpine <- memodel_data_alpine %>%
  mutate(model = map(data, model_time))

tidy_time_model_predicted_alpine <- mem_results_time_alpine %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_time_alpine <- tidy_time_model_predicted_alpine %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)

## Time - subalpine ##
mem_results_time_subalpine <- memodel_data_subalpine %>%
  mutate(model = map(data, model_time))

tidy_time_model_predicted_subalpine <- mem_results_time_subalpine %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_time_subalpine <- tidy_time_model_predicted_subalpine %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)

## Time - boreal ##
mem_results_time_boreal <- memodel_data_boreal %>%
  mutate(model = map(data, model_time))

tidy_time_model_predicted_boreal <- mem_results_time_boreal %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_time_boreal <- tidy_time_model_predicted_boreal %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)


## Anomalies ##
mem_results_anomalies <- memodel_data_fullcommunity %>%
  mutate(model = map(data, model_time_anomalies))

tidy_anomalies_model_predicted <- mem_results_anomalies %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_anomalies <- tidy_anomalies_model_predicted %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)


# Making a function for AIC tests

 AIC_models <-function(dat, model_type) {
   
   dat2 <- dat %>% 
     mutate(AIC = map(model, AIC)) %>% 
     select(Trait_trans, moments, n, AIC) %>% 
     unnest(c(AIC)) %>% 
     ungroup() %>% 
     group_by(Trait_trans, moments) %>% 
     mutate(AIC = mean(AIC)) %>% 
     select(-n) %>% 
     unique()
   
   return(dat2)
 }
 
 # Get AIC for all models and compare which is better
 
 AIC_lastyeartemp <- AIC_models(tidy_space_model_predicted_lastyeartemp, "Last year temp") %>% rename(Lastyeartemp_AIC = AIC)
 AIC_springtemp <- AIC_models(tidy_space_model_predicted_springtemp, "Spring temp") %>% rename(Springtemp_AIC = AIC)
 AIC_alltemp <- AIC_models(tidy_space_model_predicted, "full temp") %>% rename(full_AIC = AIC)
 
 AIC_all_models <- AIC_lastyeartemp %>%
   left_join(AIC_springtemp, by = c("Trait_trans" = "Trait_trans", "moments" = "moments")) %>% 
   left_join(AIC_alltemp, by = c("Trait_trans" = "Trait_trans", "moments" = "moments"))
  
#write.table(x = AIC_all_models, file = "AIC_log.csv")

# Making a dataset with the model output and the test-statistics (R squared), and summarizing them.
model_output <-function(dat) {
  
  model_output <- dat %>% 
    select(Trait_trans, moments, n, model_output, R_squared) %>% 
    #filter(Trait_trans %in% c("CN_ratio_log", "SLA_cm2_g_log") & moments %in% c("mean", "variance")) %>% 
    #filter(!Trait_trans == "CN_ratio_log" | moments == "variance") %>% 
    unnest(c(model_output, R_squared)) %>% 
    filter(term %in% c("Temp_yearly_prev", "scale(Precip_yearly)", "Temp_yearly_prev:scale(Precip_yearly)", "Temp_yearly_spring", "scale(Precip_yearly):Temp_yearly_spring")) %>% 
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
              p.value = mean(p.value)) %>% 
    mutate(Trend = case_when(CIlow.fit < 0 & CIhigh.fit < 0 ~ "Negative",
                                   CIlow.fit > 0 & CIhigh.fit > 0 ~ "Positive",
                                   CIlow.fit < 0 & CIhigh.fit > 0 ~ "No"))
  return(model_output)
}



#model_output_temp <- model_output(tidy_temp_model_predicted, c("Temp_yearly", "Temp_deviation_decade"))
#model_output_precip <- model_output(tidy_temp_model_predicted, c("Precip_yearly", "Precip_deviation_decade"))
#model_output_tempAddprecip <- model_output(tidy_tempAddprecip_model_predicted, c("Temp_yearly", "Temp_deviation_decade","scale(Precip_yearly)", "scale(Precip_deviation_decade)"))
model_output_time <- model_output(tidy_time_model_predicted)
model_output_space <- model_output(tidy_space_model_predicted)

model_output_time_forbs <- model_output(tidy_time_model_predicted_forbs)
model_output_time_graminoids <- model_output(tidy_time_model_predicted_graminoids)

model_output_time_alpine <- model_output(tidy_time_model_predicted_alpine)
model_output_time_subalpine <- model_output(tidy_time_model_predicted_subalpine)
model_output_time_boreal <- model_output(tidy_time_model_predicted_boreal)

model_output_anomalies <- tidy_anomalies_model_predicted %>% 
  select(Trait_trans, moments, n, model_output, R_squared) %>% 
  #filter(Trait_trans %in% c("CN_ratio_log", "SLA_cm2_g_log") & moments %in% c("mean", "variance")) %>% 
  #filter(!Trait_trans == "CN_ratio_log" | moments == "variance") %>% 
  unnest(c(model_output, R_squared)) %>% 
  filter(term %in% c("Temp_deviation_decade", "scale(Precip_deviation_decade)", "Temp_deviation_decade:scale(Precip_deviation_decade)")) %>% 
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
            p.value = mean(p.value)) %>% 
  mutate(Trend = case_when(CIlow.fit < 0 & CIhigh.fit < 0 ~ "Negative",
                           CIlow.fit > 0 & CIhigh.fit > 0 ~ "Positive",
                           CIlow.fit < 0 & CIhigh.fit > 0 ~ "No"))

#write.table(x = model_output, file = "model_output_TDT.csv")


#### Correlation #### Needs to be updated

# Making data ready for correlation tests

Corr_traits_17 <- summarised_boot_moments_climate_2017 %>% 
  ungroup() %>% 
  select(Site, turfID, Trait_trans, mean, Precip, Temp) %>% 
  spread(key = Trait_trans, value = mean) %>% 
  select(-Site, -turfID) 

Corr_traits_09 <- summarised_boot_moments_climate_2009 %>% 
  ungroup() %>% 
  select(Site, turfID, Trait_trans, mean, Precip, Temp) %>% 
  spread(key = Trait_trans, value = mean) %>% 
  select(-Site, -turfID) 


# Correlations 

corr_17 <- round(cor(Corr_traits_17), 1) 
head(corr_17[, 1:6])

corr_09 <- round(cor(Corr_traits_09), 1) 
head(corr_09[, 1:6])

# P-values 

p.mat_17 <- cor_pmat(Corr_traits_17)
#head(p.mat_17[, 1:4])

p.mat_09 <- cor_pmat(Corr_traits_09)
#head(p.mat_09[, 1:4])

# Correlation plot
ggcorrplot(corr_17, hc.order = FALSE,
           type = "lower", p.mat = p.mat, lab = TRUE,)

ggcorrplot(corr_09, hc.order = FALSE,
           type = "lower", p.mat = p.mat, lab = TRUE,)

#### Ordination ####

### Make data ready for ordination 

Ord_boot_traits <- Moments_fullcommunity %>% 
  left_join(env, by = c("Site" = "Site", "year" = "Year")) %>% 
  ungroup() %>% 
  mutate(uniqueID = paste0(turfID,"_", year, "_", Site),
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

Ord_boot_LeafEconomic <- Moments_fullcommunity %>% 
  left_join(env, by = c("Site" = "Site", "year" = "Year")) %>% 
  ungroup() %>% 
  mutate(uniqueID = paste0(turfID,"_", year, "_", Site)) %>% 
  group_by(uniqueID, Trait_trans) %>% 
  mutate(mean_mean = mean(mean)) %>% 
  filter(Trait_trans %in% c("SLA_cm2_g_log", "N_percent", "Leaf_Thickness_Ave_mm", "CN_ratio_log", "LDMC")) %>%  #Filter so that only leaf economic traits are in
  select(uniqueID, Site, year, turfID, Trait_trans, Temp_level, Precip_level, mean_mean) %>%
  unique() %>% 
  #gather(Moment, Value, -(turfID:P_cat)) %>% 
  #unite(temp, Trait, Moment) %>% 
  pivot_wider(names_from = Trait_trans, values_from = mean_mean) %>% 
  column_to_rownames("uniqueID")

Ord_boot_Size <- Moments_fullcommunity %>% 
  left_join(env, by = c("Site" = "Site", "year" = "Year")) %>% 
  ungroup() %>% 
  mutate(uniqueID = paste0(turfID,"_", year, "_", Site)) %>% 
  group_by(uniqueID, Trait_trans) %>% 
  mutate(mean_mean = mean(mean)) %>% 
  filter(Trait_trans %in% c("Plant_Height_mm_log", "Leaf_Area_cm2_log", "Dry_Mass_g_log", "C_percent")) %>%  #Filter so that only size traits are in
  select(uniqueID, Site, year, turfID, Trait_trans, Temp_level, Precip_level, mean_mean) %>%
  unique() %>% 
  #gather(Moment, Value, -(turfID:P_cat)) %>% 
  #unite(temp, Trait, Moment) %>% 
  pivot_wider(names_from = Trait_trans, values_from = mean_mean) %>% 
  column_to_rownames("uniqueID")

### Do ordination
pca_trait <- prcomp(Ord_boot_traits[, -(1:6)], scale = TRUE)
pca_leaf_economic <- prcomp(Ord_boot_LeafEconomic[, -(1:6)], scale = TRUE)
pca_size <- prcomp(Ord_boot_Size[, -(1:6)], scale = TRUE)

### Get variable
var <- get_pca_var(pca_trait)

### Get results
pca_trait_results <- get_pca_ind(pca_trait)
pca_leaf_economic_results <- get_pca_ind(pca_leaf_economic)
pca_size_results <- get_pca_ind(pca_size)

### Pull out axis values for different ordinations
Ord_site_env_dat <- Moments_fullcommunity %>% 
  left_join(env, by = c("Site" = "Site", "year" = "Year")) %>% 
  ungroup() %>% 
  mutate(uniqueID = paste0(turfID,"_", year, "_", Site)) %>% 
  select(uniqueID, Site, year, turfID, Temp_yearly, Precip_yearly, Temp_decade, Precip_decade, Temp_deviation_decade, Precip_deviation_decade, Temp_level, Precip_level) %>%
  unique()

Ord_axis_values_leaf_economic <- as.data.frame(pca_leaf_economic$x) %>% 
  select(PC1) %>% 
  rownames_to_column("uniqueID") %>% 
  rename(Leaf_economic_PC1 = PC1)

Ord_axis_values_size <- as.data.frame(pca_size$x) %>% 
  select(PC1) %>% 
  rownames_to_column("uniqueID") %>% 
  rename(Size_PC1 = PC1)

Ord_axis_values <- as.data.frame(pca_trait$x) %>% 
  select(PC1, PC2) %>% 
  rownames_to_column("uniqueID") %>% 
  rename(Full_ord_PC1 = PC1, Full_ord_PC2 = PC2) %>% 
  left_join(Ord_axis_values_leaf_economic, by = c("uniqueID" = "uniqueID")) %>% 
  left_join(Ord_axis_values_size, by = c("uniqueID" = "uniqueID")) %>% 
  left_join(Ord_site_env_dat, by = c("uniqueID" = "uniqueID"))

### Make data ready for model testing

Ord_modeling <- Ord_axis_values %>% 
  pivot_longer(c("Leaf_economic_PC1", "Size_PC1", "Full_ord_PC1", "Full_ord_PC2"), names_to = "ordination", values_to = "value") %>% 
  ungroup() %>%
  select(-uniqueID) %>% 
  group_by(ordination) %>% 
  nest()

### Model testing with ordiation axis

## Null ##
mem_results_null_ord <- Ord_modeling %>%
  mutate(model = map(data, model_null))

tidy_null_model_predicted_ord <- mem_results_null_ord %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_null_ord <- tidy_null_model_predicted_ord %>%
  ungroup() %>%
  select(ordination, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)


## Temp ##
mem_results_temp_ord <- Ord_modeling %>%
  mutate(model = map(data, model_temp))

tidy_temp_model_predicted_ord <- mem_results_temp_ord %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_temp_ord <- tidy_temp_model_predicted_ord %>%
  ungroup() %>%
  select(ordination, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)


## Precipitation ##
mem_results_precip_ord <- Ord_modeling %>%
  mutate(model = map(data, model_precip))

tidy_precip_model_predicted_ord <- mem_results_precip_ord %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_temp_ord <- tidy_temp_model_predicted_ord %>%
  ungroup() %>%
  select(ordination, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)

## Temperature + Precipitation (additive) ##
mem_results_tempAddprecip_ord <- Ord_modeling %>%
  mutate(model = map(data, model_tempAddprecip))

tidy_tempAddprecip_model_predicted_ord <- mem_results_tempAddprecip_ord %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_tempAddprecip_ord <- tidy_tempAddprecip_model_predicted_ord %>%
  ungroup() %>%
  select(ordination, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)

## Temperature * Precipitation (multiplicative) ##
mem_results_tempMulprecip_ord <- Ord_modeling %>%
  mutate(model = map(data, model_tempMulprecip))

tidy_tempMulprecip_model_predicted_ord <- mem_results_tempMulprecip_ord %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_tempMulprecip_ord <- tidy_tempMulprecip_model_predicted_ord %>%
  ungroup() %>%
  select(ordination, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)


# Making a function for AIC tests

AIC_models_ord <-function(dat, model_type) {
  
  dat2 <- dat %>% 
    mutate(AIC = map(model, AIC)) %>% 
    select(ordination, AIC) %>% 
    unnest(c(AIC)) %>% 
    ungroup() %>% 
    group_by(ordination) %>% 
    mutate(AIC = mean(AIC)) %>% 
    unique()
  
  return(dat2)
}

# Get AIC for all models and compare which is beter

AIC_null_ord <- AIC_models_ord(tidy_null_model_predicted_ord, "Temp") %>% rename(Null_AIC = AIC)
AIC_temp_ord <- AIC_models_ord(tidy_temp_model_predicted_ord, "Temp") %>% rename(Temp_AIC = AIC)
AIC_precip_ord <- AIC_models_ord(tidy_precip_model_predicted_ord, "Precip") %>% rename(Precip_AIC = AIC)
AIC_tempAddprecip_ord <- AIC_models_ord(tidy_tempAddprecip_model_predicted_ord, "Precip") %>% rename(TempAddPrecip_AIC = AIC)
AIC_tempMulprecip_ord <- AIC_models_ord(tidy_tempMulprecip_model_predicted_ord, "Precip") %>% rename(TempMulPrecip_AIC = AIC)

AIC_all_models_ord <- AIC_null_ord %>%
  left_join(AIC_temp_ord, by = c("ordination" = "ordination")) %>% 
  left_join(AIC_precip_ord, by = c("ordination" = "ordination")) %>% 
  left_join(AIC_tempAddprecip_ord, by = c("ordination" = "ordination")) %>% 
  left_join(AIC_tempMulprecip_ord, by = c("ordination" = "ordination"))



#### Constrained ordination ####

# RDA <- rda(Ord_boot_traits[, -(1:3)]~ Temp+Precip, scale = TRUE, data = Ord_boot_traits)
# 
# autoplot(RDA) +
#   theme_bw()
# 
# autoplot(RDA, arrows = TRUE, data = PCA_boot_traits) +
#   scale_x_continuous(expand = c(0.22, 0)) +
#   geom_point(data = RDA, aes(RDA1, RDA2), size=2) +
#   geom_abline(intercept = 0,slope = 0,linetype="dashed", size=0.8) +
#   geom_vline(aes(xintercept=0), linetype="dashed", size=0.8) + labs(x = "Axis 1", y="Axis 2") + 
#   theme_bw()
# 
# RDA_fort <- fortify(RDA)
# 
# ggplot(RDA_fort, aes(x = RDA1, y = RDA2)) +
#   geom_point(show.legend = FALSE) +
#   scale_size(range = 2) +
#   coord_equal()
# 
# 
# RDA_VPD
# plot(RDA)
# screeplot(RDA)
# 
# coef(RDA)
# 
# RsquareAdj(RDA)$adj.r.squared