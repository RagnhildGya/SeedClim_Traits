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
library(flextable)
library(officer)

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


# Trait_impute_without_intraA <- function(com_dat, trait_dat){
#   
#   trait_dat <- trait_dat  |>   
#     mutate(turfID = "")
#   
#   com_dat <- com_dat  |>   
#     filter(siteID %in% c("Hogsete", "Ulvehaugen", "Vikesland", "Gudmedalen", "Rambera", "Arhelleren"))
#   
#   SeedClim_traits <- trait_np_bootstrap(trait_fill(comm = com_dat,
#                                                      traits = trait_dat, 
#                                                      scale_hierarchy = "turfID",
#                                                      taxon_col = c("Full_name", "Genus", "Family"),
#                                                      trait_col = "Trait_trans",
#                                                      value_col = "Value",
#                                                      other_col = "year",
#                                                      abundance_col = "cover")) 
#   
#   return(SeedClim_traits)
# }
# 
# Trait_impute_without_intraB <- function(com_dat, trait_dat){
#   
#   trait_dat <- trait_dat  |>   
#     mutate(turfID = "")
#   
#   com_dat <- com_dat  |>   
#     filter(siteID %in% c("Skjelingahaugen", "Veskre", "Ovstedalen", "Alrust", "Fauske", "Lavisdalen"))
#   
#   SeedClim_traits <- trait_np_bootstrap(trait_fill(comm = com_dat,
#                                                      traits = trait_dat, 
#                                                      scale_hierarchy = "turfID",
#                                                      taxon_col = c("Full_name", "Genus", "Family"),
#                                                      trait_col = "Trait_trans",
#                                                      value_col = "Value",
#                                                      other_col = "year",
#                                                      abundance_col = "cover")) 
#   
#   return(SeedClim_traits)
# }
# 
# Imputed_traits_without_intraA <- Trait_impute_without_intraA(com_dat = community_for_boostrapping, trait_dat = traitdata)
# Imputed_traits_without_intraB <- Trait_impute_without_intraB(com_dat = community_for_boostrapping, trait_dat = traitdata)
# Imputed_traits_without_intra = bind_rows(Imputed_traits_without_intraA, Imputed_traits_without_intraB)
# 
# sum_moments_without_intraA <- trait_summarise_boot_moments(Imputed_traits_without_intraA)
# sum_moments_without_intraB <- trait_summarise_boot_moments(Imputed_traits_without_intraB)
# sum_moment_without_intra = bind_rows(sum_moments_without_intraA, sum_moments_without_intraB)

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

#Function for extracting phi from the model with autocorrelation
extract_phi <- function(model) {
  coef(model$modelStruct$corStruct, unconstrained = FALSE)
}

#Model testing traits and how they vary with spatial climate
model_climate <- function(df) {
  
  df <- df |> 
    select(siteID, turfID, Temp_decade, Precip_decade, value, year) |> 
    unique()
  
  lme(value ~ Temp_decade * Precip_decade,
      random = ~ 1|siteID/turfID,
      data = df)
}

#Functionsl for model selection

# Define the models for climate
climate_models <- list(
  "Temp * Precip" = value ~ Temp_decade * Precip_decade,
  "Temp + Precip" = value ~ Temp_decade + Precip_decade,
  "Temp" = value ~ Temp_decade,
  "Precip" = value ~ Precip_decade
)

# Define the models for temporal
temporal_models <- list(
  "Temp * Precip * Year" = value ~ Temp_decade * Precip_decade * year,
  "Temp * Precip + Year" = value ~ Temp_decade * Precip_decade + year,
  "Temp * Year + Precip * Year" = value ~ Temp_decade * year + Precip_decade * year,
  "Temp * Year + Precip + Year" = value ~ Temp_decade * year + Precip_decade + year,
  "Temp + Precip * Year + Year" = value ~ Temp_decade + Precip_decade * year + year,
  "Temp + Precip + Year" = value ~ Temp_decade + Precip_decade + year,
  "Temp + Year" = value ~ Temp_decade + year,
  "Precip + Year" = value ~ Precip_decade + year
)

#Define random effects
random_effects <- as.formula("~ 1|siteID/turfID")

#Fit and compare models
fit_and_compare_models <- function(df, models, random_effects, correlation = NULL, model_type) {
  results <- map(models, ~ {
    model <- lme(.x, random = random_effects, correlation = correlation, data = df, na.action = na.omit)
    fixed_effects <- as.character(formula(model))[3]
    list(fixed_effects = fixed_effects, model = model, AIC = AIC(model), model_type = model_type)
  })
  best_model <- results[[which.min(map_dbl(results, "AIC"))]]
  return(list(best_model = best_model$model, all_AICs = results))
}

models <- model_data |>  
  mutate(
    climateModelResults = map(data, ~ fit_and_compare_models(.x, climate_models, random_effects, model_type = "climate")),
    temporalModelResults = map(data, ~ fit_and_compare_models(.x, temporal_models, random_effects, correlation = corAR1(form = ~ year | siteID/turfID), model_type = "temporal"))
  ) |>
  mutate(
    bestClimateModel = map(climateModelResults, "best_model"),
    bestTemporalModel = map(temporalModelResults, "best_model"),
    climateAICs = map(climateModelResults, ~ map(.x$all_AICs, ~ list(fixed_effects = .x$fixed_effects, AIC = .x$AIC, model_type = .x$model_type))),
    temporalAICs = map(temporalModelResults, ~ map(.x$all_AICs, ~ list(fixed_effects = .x$fixed_effects, AIC = .x$AIC, model_type = .x$model_type)))
  )

models <- models |> 
  mutate(phi = purrr::map(bestTemporalModel, extract_phi)) |> 
  mutate(model_outputClimate = purrr::map(bestClimateModel, tidy)) |> 
  mutate(model_outputTemporal = purrr::map(bestTemporalModel, tidy))

# Functions to get output from models
outputTemporal <-function(dat, unnesting) {
  unnesting_sym <- sym(unnesting)
  
  model_output <- dat  |> 
    unnest(phi) |> 
    select(!!unnesting_sym, model_outputTemporal, phi)  |>   
    unnest(model_outputTemporal)  |>  
    select(!!unnesting_sym, term, estimate, std.error, statistic, df, p.value, phi)  |>   
    ungroup() |> 
    mutate(model = "year") |> 
    mutate(estimate = round(estimate, digits = 2)) |> 
    mutate(std.error = round(std.error, digits = 2)) |> 
    mutate(statistic = round(statistic, digits = 2)) |> 
    mutate(p.value = round(p.value, digits = 5)) |> 
    mutate(phi = round(phi, digits = 3))
  
  return(model_output)
}

# outputTemporal_com <-function(dat) {
#   
#   model_output <- dat  |> 
#     unnest(phi) |> 
#     select(community_properties, model_outputTemporal, phi)  |>   
#     unnest(model_outputYear)  |>  
#     select(community_properties, term, estimate, std.error, statistic, df, p.value, phi)  |>   
#     ungroup() |> 
#     mutate(estimate = round(estimate, digits = 2)) |> 
#     mutate(std.error = round(std.error, digits = 2)) |> 
#     mutate(statistic = round(statistic, digits = 2)) |> 
#     mutate(p.value = round(p.value, digits = 5)) |> 
#     mutate(phi = round(phi, digits = 3))
#   
#   return(model_output)
# }

outputClimate <-function(dat, unnesting) {
  unnesting_sym <- sym(unnesting)
  
  model_output <- dat  |> 
    select(!!unnesting_sym, model_outputClimate)  |>   
    unnest(model_outputClimate)  |>  
    select(!!unnesting_sym, term, estimate, std.error, statistic, df, p.value)  |>   
    ungroup() |> 
    mutate(model = "climate")
  
  return(model_output)
}

output <-function(dat, unnesting) {
  
  dat1 <- outputTemporal(dat, unnesting)
  dat2 <- outputClimate(dat, unnesting)
  
  dat3 <- dat1 |> 
    bind_rows(dat2) |> 
    mutate(estimate = round(estimate, digits = 2)) |>
    mutate(std.error = round(std.error, digits = 2)) |>
    mutate(statistic = round(statistic, digits = 2)) |>
    mutate(p.value = round(p.value, digits = 5)) |>
    mutate(phi = round(phi, digits = 3)) |> 
    mutate(significant = case_when(p.value > 0.05 ~ "NO",
                                   p.value < 0.05 ~ "YES",
                                   ))
  
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
# Temp_decade_vec <- seq(5.5, 12, length.out = 50)
# Precip_decade_vec <- c(1.0, 1.45, 2.4, 3.5)
# 
# pred <- expand.grid(Temp_decade = Temp_decade_vec,
#                      Precip_decade = Precip_decade_vec)
  
# models <- model_data |>  
#   mutate(modelYear = purrr::map(data, model_year)) |>
#   mutate(modelClimate = purrr::map(data, model_climate)) |> 
#   mutate(modelClimate2 = purrr::map(data, model_climate2)) |> 
#   mutate(modelTemp = purrr::map(data, model_temp)) |> 
#   mutate(model_outputYear = purrr::map(modelYear, tidy)) |> 
#   mutate(phi = purrr::map(modelYear, extract_phi)) |> 
#   mutate(model_outputClimate = purrr::map(modelClimate, tidy)) |>   
#   mutate(model_outputClimate2 = purrr::map(modelClimate2, tidy)) |> 
#   mutate(model_outputTemp = purrr::map(modelTemp, tidy)) |> 
#   mutate(predicted_newdata = list(pred)) |>
#   mutate(predicted_newdata = map2(predicted_newdata, modelClimate, ~ .x |> 
#                                     mutate(
#                                       predicted = predict(.y, newdata = .x, level = 0, se.fit = TRUE)$fit,
#                                       se_predicted = predict(.y, newdata = .x, level = 0, se.fit = TRUE)$se.fit))) |> 
#   mutate(data = map2(data, modelYear, ~ .x |> 
#                        mutate(predicted_year = predict(.y, newdata = .x, level = 0, se.fit = TRUE)$fit,
#                               se_predicted_year = predict(.y, newdata = .x, level = 0, se.fit = TRUE)$se.fit))) |> 
#   
#   # mutate(data = map2(data, modelYear, ~ {
#   #   preds <- predict(.y, newdata = .x, level = 1, se.fit = TRUE)
#   #   .x |> 
#   #     mutate(predicted_year = preds$fit,
#   #            se_predicted_year = preds$se.fit)
#   # }))
#   # 
#   # mutate(predictions = map2(data, modelYear, predict(modelYear, newdata = data, level = 1, se.fit =TRUE)))
# 
# #This code does predict, but not making great lines.
#    mutate(data = map2(data, modelYear, ~ .x |> 
#                        mutate(predicted_year = predict(.y, newdata = .x, level = 1, se.fit = TRUE)))) |> 
#   mutate(data = map2(data, modelClimate, ~ .x |> 
#                        mutate(predicted_climate = predict(.y, newdata = .x, level = 1, se.fit = TRUE))))
#   


models_output <- output(models, unnesting = "Trait_trans")


#write.table(models_output, row.names = TRUE, col.names = TRUE, file = "model_output_traits_AfterModelSelection.csv")


#### Get AIC for every model ----

models_AIC_climate <- models |> 
  select(Trait_trans, climateAICs) |> 
  unnest_longer(climateAICs) |> 
  unnest_wider(climateAICs) |> 
  select(Trait_trans, fixed_effects, AIC, model_type)

models_AIC_temporal <- models |> 
  select(Trait_trans, temporalAICs) |> 
  unnest_longer(temporalAICs) |> 
  unnest_wider(temporalAICs) |> 
  select(Trait_trans, fixed_effects, AIC, model_type)

models_AIC <- models_AIC_climate |> 
  bind_rows(models_AIC_temporal)


## Make a table of this for the paper 

table_AIC <- flextable(models_AIC)

# Create a Word document and add the flextable
# doc <- read_docx() |> 
#   body_add_flextable(table_AIC) |> 
#   body_add_par("AIC Values for Different Models", style = "heading 1")

# Save the Word document
#print(doc, target = "models_AIC_table.docx")
#Don't override now (I made some pretty-changes :) )


#### Running models - community ####

#Running the mixed effect model

results_com <- com_data |>  
  mutate(modelYear = purrr::map(data, model_year)) |> 
  mutate(model_outputYear = purrr::map(modelYear, tidy)) |> 
  mutate(phi = purrr::map(modelYear, extract_phi))

output_com <- outputYear_com(results_com)


models_com <- com_data |>  
  mutate(
    climateModelResults = map(data, ~ fit_and_compare_models(.x, climate_models, random_effects, model_type = "climate")),
    temporalModelResults = map(data, ~ fit_and_compare_models(.x, temporal_models, random_effects, correlation = corAR1(form = ~ year | siteID/turfID), model_type = "temporal"))
  ) |>
  mutate(
    bestClimateModel = map(climateModelResults, "best_model"),
    bestTemporalModel = map(temporalModelResults, "best_model"),
    climateAICs = map(climateModelResults, ~ map(.x$all_AICs, ~ list(fixed_effects = .x$fixed_effects, AIC = .x$AIC, model_type = .x$model_type))),
    temporalAICs = map(temporalModelResults, ~ map(.x$all_AICs, ~ list(fixed_effects = .x$fixed_effects, AIC = .x$AIC, model_type = .x$model_type)))
  )

models_com <- models_com |> 
  mutate(phi = purrr::map(bestTemporalModel, extract_phi)) |> 
  mutate(model_outputClimate = purrr::map(bestClimateModel, tidy)) |> 
  mutate(model_outputTemporal = purrr::map(bestTemporalModel, tidy))

models_com_output <- output(models_com, unnesting = "community_properties")

#write.table(models_com_output, row.names = TRUE, col.names = TRUE, file = "model_output_communityNEW.csv")

#### Get AIC for every model ----

models_com_AIC_climate <- models_com |> 
  select(community_properties, climateAICs) |> 
  unnest_longer(climateAICs) |> 
  unnest_wider(climateAICs) |> 
  select(community_properties, fixed_effects, AIC, model_type)

models_com_AIC_temporal <- models_com |> 
  select(community_properties, temporalAICs) |> 
  unnest_longer(temporalAICs) |> 
  unnest_wider(temporalAICs) |> 
  select(community_properties, fixed_effects, AIC, model_type)

models_com_AIC <- models_com_AIC_climate |> 
  bind_rows(models_com_AIC_temporal)


## Make a table of this for the paper 

table_com_AIC <- flextable(models_com_AIC)

# # Create a Word document and add the flextable
#  doc <- read_docx() |> 
#    body_add_flextable(table_com_AIC)
# 
# # Save the Word document
# print(doc, target = "models_com_AIC_table.docx")
# #Don't override now (I made some pretty-changes :) )


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
  select(uniqueID, siteID, year, templevel_year, turfID, Trait_trans, Temp_level, Precip_level, mean_mean)  |>  
  unique()  |>   
  pivot_wider(names_from = Trait_trans, values_from = mean_mean)  |>   
  column_to_rownames("uniqueID")  |>   
  rename("Leaf C "= "C_percent", "Leaf C/N" = "CN_ratio", "Dry mass" = "Dry_Mass_g_log", "Leaf area" = "Leaf_Area_cm2_log", "Leaf thickness" = "Leaf_Thickness_Ave_mm", "Leaf N" = "N_percent", "Plant height" = "Plant_Height_mm_log", "SLA" = "SLA_cm2_g")

### Do ordination
pca_trait <- prcomp(Ord_boot_traits[, -(1:6)], scale = TRUE)

### Get variable
var <- get_pca_var(pca_trait)

### Get results
pca_trait_results <- get_pca_ind(pca_trait)

#### Constrained ordination ####
RDA_null <- rda(Ord_boot_traits[, -(1:6)] ~ 1, scale = TRUE, data = Ord_boot_traits)
RDA_temp <- rda(Ord_boot_traits[, -(1:6)] ~ Temp_level, scale = TRUE, data = Ord_boot_traits)
RDA_precip <- rda(Ord_boot_traits[, -(1:6)] ~ Precip_level, scale = TRUE, data = Ord_boot_traits)
RDA_year <- rda(Ord_boot_traits[, -(1:6)] ~ year, scale = TRUE, data = Ord_boot_traits)
RDA_space_additive <- rda(Ord_boot_traits[, -(1:6)] ~ Temp_level+Precip_level, scale = TRUE, data = Ord_boot_traits)
RDA_space <- rda(Ord_boot_traits[, -(1:6)] ~ Temp_level*Precip_level, scale = TRUE, data = Ord_boot_traits)
RDA_add_year <- rda(Ord_boot_traits[, -(1:6)] ~ Temp_level*Precip_level+year, scale = TRUE, data = Ord_boot_traits)
RDA_multi_year <- rda(Ord_boot_traits[, -(1:6)] ~ Temp_level*Precip_level*year, scale = TRUE, data = Ord_boot_traits)

#Set seed and permutations to get the same p-value every time
set.seed(43)
perm <- how(nperm = 20000)

#Testing against the null model
anova(RDA_null, RDA_year, permutations = perm)
anova(RDA_null, RDA_temp, permutations = perm)
anova(RDA_null, RDA_precip, permutations = perm)

#Testing temp alone against temp + precip and temp * precip
anova(RDA_temp, RDA_space_additive)
anova(RDA_temp, RDA_space)

#Testing precip alone against temp + precip and temp * precip
anova(RDA_precip, RDA_space_additive)
anova(RDA_precip, RDA_space)

#Testing temp + precip against temp* precip
anova(RDA_space_additive, RDA_space)

#Testing space alone and space + year
anova(RDA_space, RDA_add_year, permutations = perm)

#Testing space alone and space * year
anova(RDA_space, RDA_multi_year, permutations = perm)

#Getting R2
RsquareAdj(RDA_space_additive)$adj.r.squared
RsquareAdj(RDA_space)$adj.r.squared
RsquareAdj(RDA_add_year)$adj.r.squared
RsquareAdj(RDA_multi_year)$adj.r.squared
