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

set.seed(47)

#### Making data ready for traitstrap and merging ####

## Community ##

community <- community %>% 
  filter(!year == "2010")

## Trait data ##

traitdata_2 <- traitdata_1 %>% 
  mutate(blockID = "",
         turfID = "") 

rm(traitdata_1)

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

Imputed_traits_fullcommunity <- Trait_impute_per_year(com_dat = community, trait_dat = traitdata_2) %>% 
  group_by(year, siteID, blockID, turfID, Trait_trans)

sum_moments_fullcommunity <- trait_summarise_boot_moments(Imputed_traits_fullcommunity)

#traitstrap:::autoplot.imputed_trait(Imputed_traits_fullcommunity) 
#traitstrap:::autoplot.imputed_trait(Imputed_traits_without_intra) 

#Write function for trait imputations without the intraspecific variability by sampling across sites instead of just from that site
# Trait_impute_without_intra <- function(com_dat, trait_dat){
#   
#   com_dat <- com_dat %>% 
#     mutate(random = "")
#   
#   trait_dat <- trait_dat %>% 
#     mutate(random = "")
#   
#   SeedClim_traits <- trait_np_bootstrap(trait_impute(comm = com_dat,
#                                   traits = trait_dat, 
#                                   scale_hierarchy = c("siteID","random", "blockID", "turfID"),
#                                   taxon_col = c("Full_name", "Genus", "Family"),
#                                   trait_col = "Trait_trans",
#                                   value_col = "Value",
#                                   other_col = "year",
#                                   abundance_col = "cover",
#                                   global = TRUE)) 
#   
#   return(SeedClim_traits)
# }
# 
# Imputed_traits_without_intra <- Trait_impute_without_intra(com_dat = community, trait_dat = traitdata_2) %>% 
#   group_by(year, siteID, blockID, turfID, Trait_trans)



#### Bootstraping community weighted means and making summarised moments of the distributions ####

# Moments_without_intra <- trait_np_bootstrap(imputed_traits = Imputed_traits_without_intra)
# sum_moments_without_intra <- trait_summarise_boot_moments(Imputed_traits_without_intra)
# 
# Moments_fullcommunity <- trait_np_bootstrap(imputed_traits = Imputed_traits_fullcommunity)
# sum_moments_fullcommunity <- trait_summarise_boot_moments(Imputed_traits_fullcommunity)


#### Adding climate info & pivoting longer ####

sum_moments_climate_fullcommunity = bind_rows(
  sum_moments_fullcommunity %>% 
     left_join(env, by = c("siteID" = "siteID", "year" = "Year")))

# sum_moments_climate_without_intra = bind_rows(
#   sum_moments_without_intra %>% 
#     left_join(env, by = c("siteID" = "siteID", "year" = "Year")))



moments_clim_long_fullcommunity <- Imputed_traits_fullcommunity %>% 
  pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value") %>% 
  left_join(env, by = c("siteID" = "siteID", "year" = "Year")) %>% 
  mutate(year = as.factor(year))

# moments_clim_long_without_intra <- Moments_without_intra %>% 
#   pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value") %>% 
#   left_join(env, by = c("siteID" = "siteID", "year" = "Year")) %>% 
#   mutate(year = as.factor(year))


#### Making data set with with summary information about moments in the data set ####




#### Mixed effect model testing ####

### Making dataset for models ###
memodel_data_fullcommunity <- moments_clim_long_fullcommunity %>% 
  #filter(Trait_trans %in% c("CN_ratio_log", "Leaf_Area_cm2_log", "Plant_Height_mm_log", "SLA_cm2_g_log")) %>% 
  ungroup() %>%
  select(Trait_trans, moments, siteID, turfID, Temp_yearly_prev, Temp_yearly_spring, Precip_yearly, value, year, n) %>% 
  group_by(Trait_trans, moments, n) %>% 
  nest()

# memodel_data_without_intra <- moments_clim_long_without_intra %>% 
#   #filter(Trait_trans %in% c("CN_ratio_log", "Leaf_Area_cm2_log", "Plant_Height_mm_log", "SLA_cm2_g_log")) %>% 
#   ungroup() %>%
#   select(Trait_trans, moments, siteID, turfID, Temp_yearly_prev, Temp_yearly_spring, Precip_yearly, value, year, n) %>% 
#   group_by(Trait_trans, moments, n) %>% 
#   nest()

### Funcitons for different models and model predictions later ###


model_time<-function(df) {
  lmer(value ~ Temp_yearly_spring * scale(Precip_yearly) + (1 | siteID), data = df)
}

model_space<-function(df) {
  lmer(value ~  Temp_yearly_spring * scale(Precip_yearly) + (1 | year), data = df)
}

model_space_linear<-function(df) {
  lm(value ~  Temp_yearly_spring * scale(Precip_yearly) +  year, data = df)
}

model_time_linear<-function(df) {
  lm(value ~ Temp_yearly_spring * scale(Precip_yearly) +  siteID, data = df)
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

## Space mixed ##
mem_results_space <- memodel_data_fullcommunity %>%
  filter(moments %in% c("mean", "skewness")) %>% 
  mutate(model = purrr::map(data, model_space))

tidy_space_model_predicted_mixed <- mem_results_space %>%
  mutate(model_output = purrr::map(model, tidy)) %>%
  mutate(R_squared = purrr::map(model, rsquared))

predicted_values_space_mixed <- tidy_space_model_predicted %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)

## Space linear ##
mem_results_space_linear <- memodel_data_fullcommunity %>%
  filter(moments %in% c("mean", "skewness"),
         n == 4) %>% 
  mutate(model = purrr::map(data, model_space_linear))

tidy_space_model_predicted_linear <- mem_results_space_linear %>%
  mutate(model_output = purrr::map(model, tidy)) %>% 
  mutate(predicted = purrr::map(model, predict)) %>% 
  mutate(R_squared = purrr::map(model, rsquared))

predicted_values_space_ <- tidy_space_model_predicted %>%
   ungroup() %>%
   select(Trait_trans, moments, data, predicted) %>%
   unnest(c(data, predicted)) %>%
   rename(modeled = predicted, measured = value)


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


## Time - Full community ##
mem_results_time_mixed <- memodel_data_fullcommunity %>%
  filter(moments %in% c("mean", "skewness")) %>% 
  mutate(model = purrr::map(data, model_time))

tidy_time_model_predicted_mixed <- mem_results_time_mixed %>%
  mutate(model_output = purrr::map(model, tidy)) %>%
  mutate(predicted = purrr::map(model, predict_with_random)) %>% 
  mutate(R_squared = purrr::map(model, rsquared))

predicted_values_time <- tidy_time_model_predicted %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)

## Time linear ##
mem_results_time_linear <- memodel_data_fullcommunity %>%
  filter(moments %in% c("mean", "skewness"),
         n == 4) %>% 
  mutate(model = purrr::map(data, model_time_linear))

tidy_time_model_predicted_linear <- mem_results_time_linear %>%
  mutate(model_output = purrr::map(model, tidy)) %>% 
  mutate(predicted = purrr::map(model, predict)) %>% 
  mutate(R_squared = purrr::map(model, rsquared))


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
model_output_mixed <-function(dat) {
  
  model_output <- dat %>% 
    select(Trait_trans, moments, n, model_output, R_squared) %>% 
    #filter(Trait_trans %in% c("CN_ratio_log", "SLA_cm2_g_log") & moments %in% c("mean", "variance")) %>% 
    #filter(!Trait_trans == "CN_ratio_log" | moments == "variance") %>% 
    unnest(c(model_output, R_squared)) %>% 
    filter(term %in% c("scale(Precip_yearly)", "Temp_yearly_spring", "Temp_yearly_spring:scale(Precip_yearly)")) %>% 
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

model_output_linear <-function(dat) {
  
  model_output <- dat %>% 
    select(Trait_trans, moments, n, model_output, R_squared) %>% 
    #filter(Trait_trans %in% c("CN_ratio_log", "SLA_cm2_g_log") & moments %in% c("mean", "variance")) %>% 
    #filter(!Trait_trans == "CN_ratio_log" | moments == "variance") %>% 
    unnest(c(model_output, R_squared)) %>% 
    filter(term %in% c("scale(Precip_yearly)", "Temp_yearly_spring", "Temp_yearly_spring:scale(Precip_yearly)")) %>% 
    select(Trait_trans, moments, term, n, estimate, std.error, statistic, p.value, R.squared) %>% 
    ungroup() %>% 
    group_by(Trait_trans, moments, term) %>% 
    summarize(effect = mean(estimate),
              R2 = mean(R.squared),
              #CIlow.fit = effect - sd(estimate),
              #CIhigh.fit = effect + sd(estimate),
              std.error = mean(std.error),
              staticstic = mean(statistic),
              p.value = mean(p.value))
    # mutate(Trend = case_when(CIlow.fit < 0 & CIhigh.fit < 0 ~ "Negative",
    #                          CIlow.fit > 0 & CIhigh.fit > 0 ~ "Positive",
    #                          CIlow.fit < 0 & CIhigh.fit > 0 ~ "No"))
  return(model_output)
}

#model_output_temp <- model_output(tidy_temp_model_predicted, c("Temp_yearly", "Temp_deviation_decade"))
#model_output_precip <- model_output(tidy_temp_model_predicted, c("Precip_yearly", "Precip_deviation_decade"))
#model_output_tempAddprecip <- model_output(tidy_tempAddprecip_model_predicted, c("Temp_yearly", "Temp_deviation_decade","scale(Precip_yearly)", "scale(Precip_deviation_decade)"))
model_output_time_mixed <- model_output_mixed(tidy_time_model_predicted_mixed)
model_output_time_linear <- model_output_linear(tidy_time_model_predicted_linear)

model_output_space_mixed <- model_output_mixed(tidy_space_model_predicted_mixed)
model_output_space_linear <- model_output_linear(tidy_space_model_predicted_linear)

#model_output_space_without_intra <- model_output(tidy_space_model_predicted_without_intra)

# model_output_time_forbs <- model_output(tidy_time_model_predicted_forbs)
# model_output_time_graminoids <- model_output(tidy_time_model_predicted_graminoids)
# 
# model_output_time_alpine <- model_output(tidy_time_model_predicted_alpine)
# model_output_time_subalpine <- model_output(tidy_time_model_predicted_subalpine)
# model_output_time_boreal <- model_output(tidy_time_model_predicted_boreal)

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
  model <- lmer(value ~ Temp_yearly_prev * scale(Precip_yearly) + (1 | year), data = dat2)
  
  return(model)
}

models_trait_predictions <-function(model) {

  newdata <- expand.grid(Precip_yearly=c(600, 1500, 2300, 3500), Temp_yearly_prev=seq(5.5,14, length=500), year = c(2009, 2011, 2012, 2013, 2015, 2017))
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE)
  
  return(newdata)
}

SLA_mean_sum <- model_trait_summary(memodel_data_fullcommunity, "SLA_cm2_g_log", "mean")
SLA_mean_pred <- models_trait_predictions(SLA_mean_sum)
SLA_skew_sum <- model_trait_summary(memodel_data_fullcommunity, "SLA_cm2_g_log", "skewness")
SLA_skew_pred <- models_trait_predictions(SLA_skew_sum)

CN_ratio_mean_sum <- model_trait_summary(memodel_data_fullcommunity, "CN_ratio_log", "mean")
CN_ratio_mean_pred <- models_trait_predictions(memodel_data_fullcommunity, "CN_ratio_log", "mean")
CN_ratio_skew_sum <- model_trait_summary(memodel_data_fullcommunity, "CN_ratio_log", "skewness")
CN_ratio_skew_pred <- models_trait_predictions(memodel_data_fullcommunity, "CN_ratio_log", "skewness")

LA_mean_sum <- model_trait_summary(memodel_data_fullcommunity, "Leaf_Area_cm2_log", "mean")
LA_mean_pred <- models_trait_predictions(LA_mean_sum)
LA_skew_sum <- model_trait_summary(memodel_data_fullcommunity, "Leaf_Area_cm2_log", "skewness")
LA_skew_pred <- models_trait_predictions(LA_skew_sum)

C_mean_sum <- model_trait_summary(memodel_data_fullcommunity, "C_percent", "mean")
C_mean_pred <- models_trait_predictions(C_mean_sum)
C_skew_sum <- model_trait_summary(memodel_data_fullcommunity, "C_percent", "skewness")
C_skew_pred <- models_trait_predictions(C_skew_sum)





## Run model on one of the boostrappings of the mean of SLA to make a figure ##
SLA_mean <- memodel_data_fullcommunity %>%
  filter(Trait_trans == "SLA_cm2_g_log",
         moments == "mean",
         n == 90) %>% 
  unnest(data) %>% 
  ungroup()

### Space 
mem_results_SLA_mean <- lmer(value ~ Temp_yearly_spring * scale(Precip_yearly) + (1 | year), data = SLA_mean)
summary(mem_results_SLA_mean)

newdata_SLA<-expand.grid(Precip_yearly=seq(400,6000, length=500), Temp_yearly_spring=c(6.5, 9.1, 10.9), year = c(2009, 2011, 2012, 2013, 2015, 2017))

newdata_SLA$predicted <- predict(object = mem_results_SLA_mean, newdata = newdata_SLA, re.form = NA, allow.new.levels=TRUE)

###Time
mem_results_time_SLA_mean <- lmer(value ~ Temp_yearly_prev * scale(Precip_yearly) + (1 | siteID), data = SLA_mean)

summary(mem_results_time_SLA_mean)

newdata_time_SLA<-expand.grid(Precip_yearly=seq(400,6000, length=500), Temp_yearly_prev=c(6.5, 9.1, 10.9), year = c(2009, 2011, 2012, 2013, 2015, 2017), Site = NA)

newdata_time_SLA$predicted <- predict(object = mem_results_time_SLA_mean, newdata = newdata_time_SLA, re.form = NA, allow.new.levels=TRUE)


### Skewness SLA

SLA_skewness <- memodel_data_fullcommunity %>%
  filter(Trait_trans == "SLA_cm2_g_log",
         moments == "skewness",
         n == 90) %>% 
  unnest(data) %>% 
  ungroup()

### Space 
mem_results_SLA_skewness <- lmer(value ~ Temp_yearly_spring * scale(Precip_yearly) + (1 | year), data = SLA_skewness)

summary(mem_results_SLA_skewness)

newdata_SLA_skewness <- expand.grid(Precip_yearly=seq(400,6000, length=500), Temp_yearly_spring=c(6.5, 9.1, 10.9), year = c(2009, 2011, 2012, 2013, 2015, 2017))
newdata_SLA_skewness <- expand.grid(Precip_yearly=c(600, 1500, 2300, 3500), Temp_yearly_spring=seq(4,12, length=500), year = c(2009, 2011, 2012, 2013, 2015, 2017))

newdata_SLA_skewness$predicted <- predict(object = mem_results_SLA_skewness, newdata = newdata_SLA_skewness, re.form = NA, allow.new.levels=TRUE)



## Run model on one of the boostrappings of the mean of LDMC to make a figure ##
LDMC_mean <- memodel_data_fullcommunity %>%
  filter(Trait_trans == "LDMC",
         moments == "mean",
         n == 90) %>% 
  unnest(data) %>% 
  ungroup()

mem_results_LDMC_mean <- lmer(value ~ Temp_yearly_spring * scale(Precip_yearly) + (1 | year), data = LDMC_mean)

summary(mem_results_LDMC_mean)

newdata_LDMC<-expand.grid(Precip_yearly=seq(400,6000, length=500), Temp_yearly_spring=c(6.5, 9.1, 10.9), year = c(2009, 2011, 2012, 2013, 2015, 2017))

newdata_LDMC$predicted <- predict(object = mem_results_LDMC_mean, newdata = newdata_LDMC, re.form = NA, allow.new.levels=TRUE)


### Skewness LDMC 

LDMC_skewness <- memodel_data_fullcommunity %>%
  filter(Trait_trans == "LDMC",
         moments == "skewness",
         n == 90) %>% 
  unnest(data) %>% 
  ungroup()

### Space 
mem_results_LDMC_skewness <- lmer(value ~ Temp_yearly_spring * scale(Precip_yearly) + (1 | year), data = LDMC_skewness)

summary(mem_results_LDMC_skewness)

newdata_LDMC_skewness <- expand.grid(Precip_yearly=c(600, 1500, 2300, 3500), Temp_yearly_spring=seq(4,12, length=500), year = c(2009, 2011, 2012, 2013, 2015, 2017))

newdata_LDMC_skewness$predicted <- predict(object = mem_results_LDMC_skewness, newdata = newdata_LDMC_skewness, re.form = NA, allow.new.levels=TRUE)




## Run model on one of the boostrappings of the mean of plant height to make a figure ##
Plant_height_mean <- memodel_data_fullcommunity %>%
  filter(Trait_trans == "Plant_Height_mm_log",
         moments == "mean",
         n == 90) %>% 
  unnest(data) %>% 
  ungroup()

mem_results_height_mean <- lmer(value ~ Temp_yearly_spring * scale(Precip_yearly) + (1 | year), data = Plant_height_mean)

newdata_height<-expand.grid(Precip_yearly=seq(400,6000, length=500), Temp_yearly_spring=c(6.5, 9.1, 10.9), year = c(2009, 2011, 2012, 2013, 2015, 2017))

newdata_height$predicted <- predict(object = mem_results_height_mean, newdata = newdata_height, re.form = NA, allow.new.levels=TRUE)

### Skewness height 

Plant_height_skewness <- memodel_data_fullcommunity %>%
  filter(Trait_trans == "Plant_Height_mm_log",
         moments == "skewness",
         n == 90) %>% 
  unnest(data) %>% 
  ungroup()

### Space 
mem_results_height_skewness <- lmer(value ~ Temp_yearly_spring * scale(Precip_yearly) + (1 | year), data = Plant_height_skewness)

summary(mem_results_height_skewness)

newdata_height_skewness <- expand.grid(Precip_yearly=c(600, 1500, 2300, 3500), Temp_yearly_spring=seq(4,12, length=500), year = c(2009, 2011, 2012, 2013, 2015, 2017))

newdata_height_skewness$predicted <- predict(object = mem_results_height_skewness, newdata = newdata_height_skewness, re.form = NA, allow.new.levels=TRUE)


## Run model on one of the boostrappings of the mean of leaf area to make a figure ##
Leaf_area_mean <- memodel_data_fullcommunity %>%
  filter(Trait_trans == "Leaf_Area_cm2_log",
         moments == "mean",
         n == 90) %>% 
  unnest(data) %>% 
  ungroup()

mem_results_LA_mean <- lmer(value ~ Temp_yearly_spring * scale(Precip_yearly) + (1 | year), data = Leaf_area_mean)

summary(mem_results_LA_mean)

newdata_LA<-expand.grid(Precip_yearly=seq(400,6000, length=500), Temp_yearly_spring=c(6.5, 9.1, 10.9), year = c(2009, 2011, 2012, 2013, 2015, 2017))

newdata_LA$predicted <- predict(object = mem_results_LA_mean, newdata = newdata_LA, re.form = NA, allow.new.levels=TRUE)


## Run model on one of the boostrappings of the mean of leaf area to make a figure ##
Leaf_area_skewness <- memodel_data_fullcommunity %>%
  filter(Trait_trans == "Leaf_Area_cm2_log",
         moments == "skewness",
         n == 90) %>% 
  unnest(data) %>% 
  ungroup()

mem_results_LA_skew <- lmer(value ~ Temp_yearly_spring * scale(Precip_yearly) + (1 | year), data = Leaf_area_skewness)

summary(mem_results_LA_skew)

newdata_LA_skew<-expand.grid(Precip_yearly=c(600, 1500, 2300, 3500), Temp_yearly_spring=seq(4,12, length=500), year = c(2009, 2011, 2012, 2013, 2015, 2017))

newdata_LA_skew$predicted <- predict(object = mem_results_LA_skew, newdata = newdata_LA_skew, re.form = NA, allow.new.levels=TRUE)


## Run model on one of the boostrappings of the mean of c% to make a figure ##
C_percent_mean <- memodel_data_fullcommunity %>%
  filter(Trait_trans == "C_percent",
         moments == "mean",
         n == 90) %>% 
  unnest(data) %>% 
  ungroup()

mem_results_C_mean <- lmer(value ~ Temp_yearly_spring * scale(Precip_yearly) + (1 | year), data = C_percent_mean)

summary(mem_results_C_mean)

newdata_C<-expand.grid(Precip_yearly=seq(400,6000, length=500), Temp_yearly_spring=c(6.5, 9.1, 10.9), year = c(2009, 2011, 2012, 2013, 2015, 2017))

newdata_C$predicted <- predict(object = mem_results_C_mean, newdata = newdata_C, re.form = NA, allow.new.levels=TRUE)




#### Correlation #### Needs to be updated

# Making data ready for correlation tests

Corr_traits <- sum_moments_climate_fullcommunity %>% 
  ungroup() %>% 
  select(Site, turfID, Trait_trans, mean, Precip_yearly, Temp_yearly_spring) %>% 
  spread(key = Trait_trans, value = mean) %>% 
  select(-Site, -turfID) 

# Correlations 

corr <- round(cor(Corr_traits), 1) 
head(corr[, 1:6])

# P-values 

p.mat <- cor_pmat(Corr_traits)
#head(p.mat_17[, 1:4])

# Correlation plot

ggcorrplot(corr, hc.order = FALSE,
           type = "lower", p.mat = p.mat, lab = TRUE,)


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