#### Bottstrapping and analysis for trait driver theory paper with climate in time and space ####

#### Source ####

source("R/Cleaning.R")

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
#library(ggvegan)
library(partR2)
#library(drake)
#library(default)
library(conflicted)
library(nlme)

set.seed(47)

#### Setting conflict standards ####

conflict_prefer("map", winner="purrr")
conflict_prefer("filter", winner = "dplyr")
conflict_prefer("select", winner = "dplyr")
conflict_prefer("lmer", winner = "lmerTest")

#### Making data ready for traitstrap and merging ####

## Community ##

community_for_boostrapping <- community  |>   
  filter(!year == "2010")  |>   
  select(siteID, blockID, turfID, year, species, Full_name, Genus,
         Family, Order, cover)

community_for_analysis <- community  |>   
  filter(!year == "2010")  |>   
  select(siteID, blockID, turfID, year, species, Full_name, Genus,
         Family, Order, cover, total_vascular, total_bryophytes,
         vegetation_height, moss_height, functionalGroup)

turf_site_dict <- community  |>   
  select(siteID, turfID)  |>   
  distinct()

rm(community)

## Trait data ##

traitdata <- traitdata_1  |>   
  mutate(blockID = "",
         turfID = "") 

rm(traitdata_1)

## Climate data - transform precipitation from mm to m ##

env <- env  |>   
  mutate(Precip_yearly = Precip_yearly/1000,
         Precip_yearly_spring = Precip_yearly_spring/1000,
         Precip_decade = Precip_decade/1000,
         Precip_se = Precip_se/1000,
         Precip_annomalies = Precip_annomalies/1000,
         Precip_century = Precip_century/1000)

## Climate data - making model for mean change in temp and precip ##

temp_model <- lmer(Temp_yearly_spring ~ year + (1|siteID), data = env)
precip_model <- lmer(Precip_yearly ~ year + (1|siteID), data = env)

# summary(temp_model)
# rsquared(temp_model)
# summary(precip_model)
# rsquared(precip_model)

env_predictions <-function(model_temp, model_precip) {
  
  newdata <- expand.grid(year=c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019), siteID = c("Alrust", "Arhelleren", "Fauske", "Gudmedalen", "Hogsete", "Lavisdalen", "Ovstedalen", "Rambera", "Skjellingahaugen", "Ulvehaugen", "Veskre", "Vikesland"))
  
  newdata$temp_modeled <- predict(object = model_temp, newdata = newdata, re.form = NULL, allow.new.levels=TRUE)
  newdata$precip_modeled <- predict(object = model_precip, newdata = newdata, re.form = NULL, allow.new.levels=TRUE)
  
  return(newdata)
}

env_pred <- env_predictions(temp_model, precip_model)  |>   
  mutate(siteID = as.character(siteID))


env <- env  |>   
  left_join(env_pred, by = c("siteID" = "siteID", "year" = "year"))

#### Bootstrapping ####

Trait_impute_per_year <- function(com_dat, trait_dat){
  
  SeedClim_traits <- trait_np_bootstrap(
    trait_fill(comm = com_dat,
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


Imputed_traits_fullcommunity <- Trait_impute_per_year(com_dat = community_for_boostrapping, trait_dat = traitdata)

sum_moments_fullcommunity <- trait_summarise_boot_moments(Imputed_traits_fullcommunity)


Trait_impute_without_intraA <- function(com_dat, trait_dat){
  
  trait_dat <- trait_dat  |>   
    mutate(turfID = "")
  
  com_dat <- com_dat  |>   
    filter(siteID %in% c("Hogsete", "Ulvehaugen", "Vikesland", "Gudmedalen", "Rambera", "Arhelleren"))
  
  SeedClim_traits <- trait_np_bootstrap(trait_fill(comm = com_dat,
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
  
  trait_dat <- trait_dat  |>   
    mutate(turfID = "")
  
  com_dat <- com_dat  |>   
    filter(siteID %in% c("Skjelingahaugen", "Veskre", "Ovstedalen", "Alrust", "Fauske", "Lavisdalen"))
  
  SeedClim_traits <- trait_np_bootstrap(trait_fill(comm = com_dat,
                                                     traits = trait_dat, 
                                                     scale_hierarchy = "turfID",
                                                     taxon_col = c("Full_name", "Genus", "Family"),
                                                     trait_col = "Trait_trans",
                                                     value_col = "Value",
                                                     other_col = "year",
                                                     abundance_col = "cover")) 
  
  return(SeedClim_traits)
}

Imputed_traits_without_intraA <- Trait_impute_without_intraA(com_dat = community_for_boostrapping, trait_dat = traitdata)
Imputed_traits_without_intraB <- Trait_impute_without_intraB(com_dat = community_for_boostrapping, trait_dat = traitdata)
Imputed_traits_without_intra = bind_rows(Imputed_traits_without_intraA, Imputed_traits_without_intraB)

sum_moments_without_intraA <- trait_summarise_boot_moments(Imputed_traits_without_intraA)
sum_moments_without_intraB <- trait_summarise_boot_moments(Imputed_traits_without_intraB)
sum_moment_without_intra = bind_rows(sum_moments_without_intraA, sum_moments_without_intraB)

#### Adding climate info & pivoting longer ####

sum_moments_climate_fullcommunity = bind_rows(
  sum_moments_fullcommunity  |>   
    left_join(env, by = c("siteID" = "siteID", "year" = "year")))

sum_moments_climate_without_intra = bind_rows(
  sum_moment_without_intra  |>   
    left_join(turf_site_dict, by = c("turfID" = "turfID"))  |>   
    left_join(env, by = c("siteID" = "siteID", "year" = "year")))


moments_clim_long_fullcommunity <- Imputed_traits_fullcommunity  |>   
  pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value")  |>   
  left_join(env, by = c("siteID" = "siteID", "year" = "year"))


moments_clim_long_without_intra <- Imputed_traits_without_intra  |>   
  pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value")  |>   
  left_join(turf_site_dict, by = c("turfID" = "turfID"))  |>   
  left_join(env, by = c("siteID" = "siteID", "year" = "year"))

rm(Imputed_traits_fullcommunity)
rm(Imputed_traits_without_intraA)
rm(Imputed_traits_without_intraB)
rm(Imputed_traits_without_intra)
rm(sum_moments_without_intraA)
rm(sum_moments_without_intraB)
rm(sum_moments_fullcommunity)

###### Mixed effect model testing ######

#### Making dataset for models ####

# With intraspecific variability
# memodel_data_fullcommunity <- moments_clim_long_fullcommunity  |>   
#   ungroup()  |>  
#   select(Trait_trans, moments, siteID, turfID, Temp_yearly_spring, Precip_yearly, Temp_decade, Precip_decade, Temp_annomalies, Precip_annomalies, Temp_level, Precip_level, value, year)  |>  
#   mutate(value_not_transformed = value) |> 
#   mutate(value = scale(value))  |>  
#   group_by(Trait_trans, moments)  |>   
#   nest()
# 
# rm(moments_clim_long_fullcommunity)

#Making this for the summarized dataset
model_data <- sum_moments_climate_fullcommunity  |>   
  ungroup()  |>  
  select(Trait_trans, year, siteID, blockID, turfID, 
         mean,
         Temp_yearly_spring, Precip_yearly, 
         Temp_decade, Precip_decade, 
         Temp_annomalies, Precip_annomalies, 
         Temp_level, Precip_level)  |> 
  mutate(mean_transformed = scale(mean))  |> 
  group_by(Trait_trans, siteID, turfID) |> 
  mutate(mean_decade = mean(mean)) |> 
  mutate(value = mean) |>  #To have the same name for what goes in the model for both trait and community data
  ungroup() |> 
  group_by(Trait_trans)  |>   
  nest()

# Without intraspecific variability
# memodel_data_without_intra <- moments_clim_long_without_intra  |>   
#   ungroup()  |>  
#   select(Trait_trans, moments, siteID, turfID,Temp_yearly_spring, Precip_yearly, Temp_decade, Precip_decade, Temp_annomalies, Precip_annomalies, Temp_level, Precip_level, value, year)  |>  
#   mutate(value_not_transformed = value) |> 
#   mutate(value = scale(value))  |>   
#   group_by(Trait_trans, moments)  |>   
#   nest()
# 
# rm(moments_clim_long_without_intra)

# community data

com_data <- community_for_analysis  |>  
  group_by(turfID, year)  |>   
  mutate(species_richness = n())  |>   
  unique()  |>   
  group_by(turfID, year)  |>   
  pivot_wider(names_from = functionalGroup, values_from = cover)  |>   
  mutate(graminoid_cover = sum(graminoid, na.rm = TRUE),
         forb_cover = sum(forb, na.rm = TRUE),
         other_cover = sum(woody, pteridophyte, `NA`, na.rm = TRUE))  |>   
  left_join(env, by = c("siteID" = "siteID", "year" = "year"))  |>   
  pivot_longer(cols = c("species_richness", "graminoid_cover", "forb_cover", "other_cover", "total_vascular", "total_bryophytes", "vegetation_height", "moss_height"), names_to = "community_properties", values_to = "value")  |>   
  select(siteID, turfID, Temp_yearly_spring, Precip_yearly, Temp_decade, Precip_decade, Temp_annomalies, Precip_annomalies, Temp_level, Precip_level, year, value, community_properties)  |>   
  group_by(community_properties)  |>   
  unique()  |>
  mutate(value_transformed = scale(value))  |>   
  nest()

#### Functions for models and model outputs ####


#Full model testing how traits and community properties varies with years and if that is dependent on climate
model_year <- function(df) {
  lme(value ~ Temp_decade * Precip_decade * year,
      random = ~ 1|siteID/turfID, 
      correlation = corAR1(form = ~ year | siteID/turfID),
      data = df,
      na.action = na.omit)
}


#Model testing traits and how they vary with spacial climate
model_climate <- function(df) {
  
  df <- df |> 
    select(siteID, turfID, Temp_decade, Precip_decade, mean_decade) |> 
    unique()
  
  lme(mean_decade ~ Temp_decade * Precip_decade,
      random = ~ 1|siteID/turfID,
      data = df)
}

extract_phi <- function(model) {
  coef(model$modelStruct$corStruct, unconstrained = FALSE)
}

# extract_random_effect <- function(model) {
#   coef(model$modelStruct$reStruct)
# }
#VarCorr(models[[3]][[1]])
#Not sure what to use here. What is normal to report on nested random effects?

outputYear <-function(dat) {
  
  model_output <- dat  |> 
    unnest(phi) |> 
    select(Trait_trans, model_outputYear, phi)  |>   
    unnest(model_outputYear)  |>  
    select(Trait_trans, term, estimate, std.error, statistic, df, p.value, phi)  |>   
    ungroup() |> 
    mutate(model = "year") |> 
    mutate(estimate = round(estimate, digits = 2)) |> 
    mutate(std.error = round(std.error, digits = 2)) |> 
    mutate(statistic = round(statistic, digits = 2)) |> 
    mutate(p.value = round(p.value, digits = 5)) |> 
    mutate(phi = round(phi, digits = 3))
  
  return(model_output)
}

outputYear_com <-function(dat) {
  
  model_output <- dat  |> 
    unnest(phi) |> 
    select(community_properties, model_outputYear, phi)  |>   
    unnest(model_outputYear)  |>  
    select(community_properties, term, estimate, std.error, statistic, df, p.value, phi)  |>   
    ungroup() |> 
    mutate(estimate = round(estimate, digits = 2)) |> 
    mutate(std.error = round(std.error, digits = 2)) |> 
    mutate(statistic = round(statistic, digits = 2)) |> 
    mutate(p.value = round(p.value, digits = 5)) |> 
    mutate(phi = round(phi, digits = 3))
  
  return(model_output)
}

outputClimate <-function(dat) {
  
  model_output <- dat  |> 
    select(Trait_trans, model_outputClimate)  |>   
    unnest(model_outputClimate)  |>  
    select(Trait_trans, term, estimate, std.error, statistic, df, p.value)  |>   
    ungroup() |> 
    mutate(model = "climate")
  
  return(model_output)
}

output <-function(dat) {
  
  dat1 <- outputYear(dat)
  dat2 <- outputClimate(dat)
  
  dat3 <- dat1 |> 
    bind_rows(dat2) |> 
    mutate(estimate = round(estimate, digits = 2)) |> 
    mutate(std.error = round(std.error, digits = 2)) |> 
    mutate(statistic = round(statistic, digits = 2)) |> 
    mutate(p.value = round(p.value, digits = 5)) |> 
    mutate(phi = round(phi, digits = 3))
  
  return(dat3)
}
  
# output_com <-function(dat) {
#   
#   model_output <- dat  |>   
#     select(community_properties, model_output, R_squared)  |>   
#     unnest(c(model_output, R_squared))  |>   
#     filter(!term %in% c("(Intercept)", "sd__(Intercept)", "sd__Observation"))  |>   
#     select(community_properties, term, estimate, std.error, statistic, df, p.value, Marginal, Conditional)  |>   
#     ungroup() 
#   
#   return(model_output)
# }


#### Running models - traits ####

# Running the model, tidying the model output

models <- model_data |>  
  mutate(modelYear = purrr::map(data, model_year)) |>
  mutate(modelClimate = purrr::map(data, model_climate)) |> 
  mutate(model_outputYear = purrr::map(modelYear, tidy)) |> 
  mutate(phi = purrr::map(modelYear, extract_phi)) |> 
  mutate(model_outputClimate = purrr::map(modelClimate, tidy)) |> 
  mutate(data = map2(data, modelYear, ~ .x |> 
                       mutate(predicted_year = predict(.y, newdata = .x, level = 1))))

models_output <- output(models)


#write.table(models_output, row.names = TRUE, col.names = TRUE, file = "model_output_traits.csv")


#### Running models - community ####

#Running the mixed effect model

results_com <- com_data |>  
  mutate(modelYear = purrr::map(data, model_year)) |> 
  mutate(model_outputYear = purrr::map(modelYear, tidy)) |> 
  mutate(phi = purrr::map(modelYear, extract_phi))

output_com <- outputYear_com(results_com)

#write.table(output_com, row.names = TRUE, col.names = TRUE, file = "model_output_community.csv")

  #mutate_if(is.numeric, round, digits = 5)






############# Below this is old code ###################



#### Running models - trait without intraspecific variability ####

# Running the mixed effects model

results_TDT_without_intra <- memodel_data_without_intra  |>  
  filter(moments %in% c("mean"))  |>   
  mutate(model = purrr::map(data, model_TDT))

#Tidying up the model output

tidy_TDT_without_intra <- results_TDT_without_intra  |>  
  mutate(model_output = purrr::map(model, tidy))  |>  
  mutate(R_squared = purrr::map(model, rsquared))

# Making a dataset with the model output and the test-statistics (R squared), summarizing across boootstraps.

output_TDT_without_intra <- output(tidy_TDT_without_intra)  |>   
  mutate(R2_conditional = round(R2_conditional, digits = 2),
         R2_marginal = round(R2_marginal, digits = 2),
         effect = round(effect, digits = 7),
         CIlow.fit = round(CIlow.fit, digits = 7),
         CIhigh.fit = round(CIhigh.fit, digits = 7),
         std.error = round(std.error, digits = 7),
         staticstic = round(staticstic, digits = 2),
         df = round(df, digits = 5),
         p.value = round(p.value, digits = 3)) |>   
  filter(!Trait_trans == "Wet_Mass_g_log")

#write.table(output_TDT_without_intra, row.names = TRUE, col.names = TRUE, file = "model_output_without_intra.csv")

#### Simpler mixed effect models on specific traits to make predicted plots ####


model_trait_summary <-function(dat, trait, moment) {
  
  
  # Filter data for model
  dat2 <- dat  |>  
    filter(Trait_trans == trait,
           moments == moment,
           n == 75)  |>   
    unnest(data)  |>   
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

  newdata <- expand.grid(Precip_decade = c(1.0, 1.45, 2.4, 3.5), 
                         Temp_decade = seq(5.5,12, length = 200), 
                         siteID = c("Alrust", "Arhelleren", "Fauske", "Gudmedalen", "Hogsete", "Lavisdalen", "Ovstedalen", "Rambera", "Skjelingahaugen", "Ulvehaugen", "Veskre", "Vikesland"),
                         Precip_annomalies = 0,
                         Temp_annomalies = 0)
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE)
  
  return(newdata)
}

models_trait_predictions_time <-function(model) {
  
  newdata <- expand.grid(Precip_decade = 3, 
                         Temp_decade = 6, 
                         siteID = c("Alrust", "Arhelleren", "Fauske", "Gudmedalen", "Hogsete", "Lavisdalen", "Ovstedalen", "Rambera", "Skjelingahaugen", "Ulvehaugen", "Veskre", "Vikesland"),
                         Precip_annomalies = c(-1.5, 0, 1.5, 3),
                         Temp_annomalies = seq(-3, 1.5, length = 200))
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE)
  
  return(newdata)
}

#### Get partial R2 for all models and traits ####

library(ggpubr)
library(patchwork)

 partR2_func <-function(dat, trait, moment) {
  
   dat2 <- dat  |>   
   filter(Trait_trans == trait,
          moments == moment,
          n == 75)  |>   
   unnest(data)  |>   
   ungroup() 
 
 mod <- lmer(value ~ scale(Temp_decade) + scale(Precip_decade) + scale(Temp_annomalies) + scale(Precip_annomalies) + 
               scale(Temp_decade)*scale(Precip_decade) + scale(Temp_annomalies)*scale(Precip_annomalies) + 
               scale(Temp_decade)*scale(Temp_annomalies) + scale(Temp_decade)*scale(Precip_annomalies) + 
               scale(Precip_decade)*scale(Temp_annomalies) + scale(Precip_decade)*scale(Precip_annomalies) +
               + (1|siteID), data = dat2)
 
 part_space <- partR2(mod, data = dat2, partvars = c("scale(Temp_decade)", "scale(Precip_decade)", "scale(Temp_decade):scale(Precip_decade)"), R2_type = "conditional", nboot = 10)
 
 part_time <- partR2(mod, data = dat2, partvars = c("scale(Temp_annomalies)", "scale(Precip_annomalies)", "scale(Temp_annomalies):scale(Precip_annomalies)"), R2_type = "conditional", nboot = 10)
 
 part_space_and_time <- partR2(mod, data = dat2, partbatch = list(Space_and_Time = c("scale(Temp_annomalies):scale(Temp_decade)", "scale(Precip_annomalies):scale(Precip_decade)", "scale(Temp_decade)*scale(Precip_annomalies)", "scale(Precip_decade)*scale(Temp_annomalies)")), R2_type = "conditional", nboot = 10)
 
 data <- part_space$R2  |>   
   filter(term %in% c("Full", "scale(Temp_decade)", "scale(Precip_decade)", "scale(Temp_decade):scale(Precip_decade)"))  |>   
   mutate(model = "space")
 
 data2 <- part_time$R2  |>   
   filter(term %in% c("scale(Temp_annomalies)", "scale(Precip_annomalies)", "scale(Temp_annomalies):scale(Precip_annomalies)"))  |>   
   mutate(model = "time")
 
 data3 <- part_space_and_time$R2  |>   
   filter(term == "Space_and_Time")  |>   
   mutate(model = "space_and_time")
 
 data <- data  |>   
   full_join(data2)  |>   
   full_join(data3)  |>   
   mutate(traits = trait)
 
 return(data)
 }
 
 LDMC_partR2 <- partR2_func(dat = memodel_data_fullcommunity, moment = "mean", trait = "LDMC")
 SLA_partR2 <- partR2_func(dat = memodel_data_fullcommunity, moment = "mean", trait = "SLA_cm2_g")
 Height_partR2 <- partR2_func(dat = memodel_data_fullcommunity, moment = "mean", trait = "Plant_Height_mm_log")
 CN_partR2 <- partR2_func(dat = memodel_data_fullcommunity, moment = "mean", trait = "CN_ratio")
 LA_partR2 <- partR2_func(dat = memodel_data_fullcommunity, moment = "mean", trait = "Leaf_Area_cm2_log")
 Lth_partR2 <- partR2_func(dat = memodel_data_fullcommunity, moment = "mean", trait = "Leaf_Thickness_Ave_mm")
 N_partR2 <- partR2_func(dat = memodel_data_fullcommunity, moment = "mean", trait = "N_percent")
 C_partR2 <- partR2_func(dat = memodel_data_fullcommunity, moment = "mean", trait = "C_percent")
 Mass_partR2 <- partR2_func(dat = memodel_data_fullcommunity, moment = "mean", trait = "Dry_Mass_g_log")
 
 partR2_data <- LDMC_partR2  |>   
   full_join(SLA_partR2)  |>   
   full_join(Height_partR2)  |>   
   full_join(CN_partR2)  |>   
   full_join(LA_partR2)  |>   
   full_join(Lth_partR2)  |>   
   full_join(N_partR2)  |>   
   full_join(C_partR2)  |>   
   full_join(Mass_partR2)
 
 partR2_data <- partR2_data  |>   
   group_by(traits)  |>   
   mutate(Full = first(estimate))  |>  
   ungroup()  |>   
   mutate(term = as.character(recode(term, "scale(Temp_decade)" = "Space: Temperature", "scale(Precip_decade)" = "Space: Precipitation", "scale(Temp_decade):scale(Precip_decade)" = "Space: T:P interaction", "scale(Temp_annomalies)" = "Time: Temperature", "scale(Precip_annomalies)" = "Time: Precipitation", "scale(Temp_annomalies):scale(Precip_annomalies)" = "Time: T:P Interaction", "Space_and_Time" = "Interactions Time and Space", "Full" = "Full model")))  |>   
   mutate(term = factor(term, levels = c("Interactions Time and Space", "Time: T:P Interaction", "Time: Precipitation",  "Time: Temperature","Space: T:P interaction", "Space: Precipitation",  "Space: Temperature", "Full model")))  |>   
   mutate(traits = as.character(recode(traits, "CN_ratio" = "Leaf C/N", "N_percent" = "Leaf N", "C_percent" = "Leaf C")))  |>   
   mutate(Space_or_time = case_when(term %in% c("Space: Temperature", "Space: Precipitation", "Space: T:P interaction") ~ "Space",
                                    term %in% c("Time: Temperature", "Time: Precipitation", "Time: T:P Interaction") ~ "Time",
                                    term == "Full model" ~ "Full model",
                                    term == "Interactions Time and Space" ~ "Interactions Time and Space"))  |>   
   mutate(Temp_or_precip = case_when(term %in% c("Space: Temperature", "Time: Temperature") ~ "Temp",
                                     term %in% c("Space: Precipitation", "Time: Precipitation") ~ "Precip",
                                     term %in% c("Space: T:P interaction", "Time: T:P Interaction", "Interactions Time and Space") ~ "Interactions",
                                     term == "Full model" ~ "Full model"))  |>   
   mutate(Space_or_time = factor(Space_or_time, levels = c("Full model","Space", "Time", "Interactions Time and Space")),
          Temp_or_precip = factor(Temp_or_precip, levels = c("Full model","Temp", "Precip", "Interactions")))  |>   
   mutate(proportion = estimate/Full * 100)
 
 # Partial_R2_plot <- (SLA_partR2_plot | LDMC_partR2_plot | Lth_partR2_plot | CN_partR2_plot | N_partR2_plot) /
 #   (Height_partR2_plot | LA_partR2_plot | Mass_partR2_plot | C_partR2_plot | plot_spacer())
 # 
 # ggsave("Partial_R2.pdf", width = 20 , height = 11, units = "cm", plot = Partial_R2_plot)

#### Make datasets with modeled values for different traits for plotting ####

SLA_mean_sum_yc <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "SLA_cm2_g", "mean")
SLA_mean_pred_space <- models_trait_predictions_space(SLA_mean_sum_yc)
SLA_mean_pred_time <- models_trait_predictions_time(SLA_mean_sum_yc)

LDMC_mean_sum_yc <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "LDMC", "mean")
LDMC_mean_pred_space <- models_trait_predictions_space(LDMC_mean_sum_yc)
LDMC_mean_pred_time <- models_trait_predictions_time(LDMC_mean_sum_yc)

Height_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "Plant_Height_mm_log", "mean")
Height_mean_pred_space <- models_trait_predictions_space(Height_mean_sum)

CN_ratio_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "CN_ratio", "mean")
CN_ratio_mean_pred_space <- models_trait_predictions_space(CN_ratio_mean_sum)

LA_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "Leaf_Area_cm2_log", "mean")
LA_mean_pred_space <- models_trait_predictions_space(LA_mean_sum)

C_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "C_percent", "mean")
C_mean_pred_space <- models_trait_predictions_space(C_mean_sum)

N_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "N_percent", "mean")
N_mean_pred_space <- models_trait_predictions_space(N_mean_sum)

Mass_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "Dry_Mass_g_log", "mean")
Mass_mean_pred_space <- models_trait_predictions_space(Mass_mean_sum)

LDMC_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "LDMC", "mean")
LDMC_mean_pred_space <- models_trait_predictions_space(LDMC_mean_sum)

Lth_mean_sum <- model_trait_summary(memodel_data_fullcommunity_nottransformed, "Leaf_Thickness_Ave_mm", "mean")
Lth_mean_pred_space <- models_trait_predictions_space(Lth_mean_sum)

#### Correlation ####

# Making data ready for correlation tests

Corr_traits <- sum_moments_climate_fullcommunity  |>   
  ungroup()  |>   
  select(Site, turfID, Trait_trans, mean, Precip_yearly, Temp_yearly_spring, Temp_yearly_prev,  Temp_summer, Precip_yearly_spring)  |>   
  spread(key = Trait_trans, value = mean)  |>   
  select(-Site, -turfID) 

#### Ordination ####

### Make data ready for ordination 

Ord_boot_traits <- sum_moments_climate_fullcommunity  |>   
  ungroup()  |>   
  mutate(uniqueID = paste0(turfID,"_", year, "_", siteID),
         templevel_year = paste0(Temp_level, "_", year))  |>   
  group_by(uniqueID, Trait_trans)  |>   
  mutate(mean_mean = mean(mean))  |>   
  filter(!Trait_trans == "Wet_Mass_g_log")  |>   
  select(uniqueID, siteID, year, templevel_year, turfID, Trait_trans, Temp_level, Precip_level, mean_mean, Temp_annomalies, Precip_annomalies)  |>  
  unique()  |>   
  #gather(Moment, Value, -(turfID:P_cat))  |>   
  #unite(temp, Trait, Moment)  |>   
  pivot_wider(names_from = Trait_trans, values_from = mean_mean)  |>   
  column_to_rownames("uniqueID")  |>   
  rename("Leaf C "= "C_percent", "Leaf C/N" = "CN_ratio", "Dry mass" = "Dry_Mass_g_log", "Leaf area" = "Leaf_Area_cm2_log", "Leaf thickness" = "Leaf_Thickness_Ave_mm", "Leaf N" = "N_percent", "Plant height" = "Plant_Height_mm_log", "SLA" = "SLA_cm2_g")

### Do ordination
pca_trait <- prcomp(Ord_boot_traits[, -(1:8)], scale = TRUE)

### Get variable
var <- get_pca_var(pca_trait)

### Get results
pca_trait_results <- get_pca_ind(pca_trait)

#### Constrained ordination ####
RDA_temp <- rda(Ord_boot_traits[, -(1:8)]~ Temp_level, scale = TRUE, data = Ord_boot_traits)
RDA_precip <- rda(Ord_boot_traits[, -(1:8)]~ Precip_level, scale = TRUE, data = Ord_boot_traits)
RDA_space_additive <- rda(Ord_boot_traits[, -(1:8)]~ Temp_level+Precip_level, scale = TRUE, data = Ord_boot_traits)
RDA_space <- rda(Ord_boot_traits[, -(1:8)]~ Temp_level*Precip_level, scale = TRUE, data = Ord_boot_traits)
RDA_space_add_time <- rda(Ord_boot_traits[, -(1:8)]~ Temp_level*Precip_level + Temp_annomalies*Precip_annomalies, scale = TRUE, data = Ord_boot_traits)
RDA_space_and_time <- rda(Ord_boot_traits[, -(1:8)]~ Temp_level*Precip_level * Temp_annomalies*Precip_annomalies, scale = TRUE, data = Ord_boot_traits)
RDA_add_year <- rda(Ord_boot_traits[, -(1:8)]~ Temp_level*Precip_level+year, scale = TRUE, data = Ord_boot_traits)
RDA_year <- rda(Ord_boot_traits[, -(1:8)]~ Temp_level*Precip_level*year, scale = TRUE, data = Ord_boot_traits)

#Testing temp alone against temp + precip and temp * precip
anova(RDA_temp, RDA_space_additive)
anova(RDA_temp, RDA_space)

#Testing precip alone against temp + precip and temp * precip
anova(RDA_precip, RDA_space_additive)
anova(RDA_precip, RDA_space)

#Testing temp + precip against temp* precip
anova(RDA_space_additive, RDA_space)

#Testing space alone and space + time
anova(RDA_space, RDA_space_add_time)

#Testing space alone and space * time
anova(RDA_space, RDA_space_and_time)

#Testing space alone and space + year
anova(RDA_space, RDA_add_year)

#Testing space alone and space * year
anova(RDA_space, RDA_year)

#Getting R2
RsquareAdj(RDA_space_additive)$adj.r.squared
RsquareAdj(RDA_space)$adj.r.squared
RsquareAdj(RDA_space_add_time)$adj.r.squared
RsquareAdj(RDA_space_and_time)$adj.r.squared
RsquareAdj(RDA_add_year)$adj.r.squared
RsquareAdj(RDA_year)$adj.r.squared
