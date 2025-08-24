#### Bootstrapping and analysis for trait paper with climate and temporal trends ####

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
library(ggvegan)
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

### Without intraspecific trait variability

Trait_impute_per_year_without_ITV <- function(com_dat, trait_dat){
  
  SeedClim_traits <- trait_np_bootstrap(
    trait_fill(comm = com_dat,
               traits = trait_dat,
               scale_hierarchy = c(),
               taxon_col = c("Full_name", "Genus", "Family"),
               trait_col = "Trait_trans",
               value_col = "Value",
               other_col = c("year", "siteID", "blockID", "turfID"),
               abundance_col = "cover"))
  
  return(SeedClim_traits)
}


Imputed_traits_fullcommunity_without_ITV <- Trait_impute_per_year_without_ITV(com_dat = community_for_boostrapping, trait_dat = traitdata)

sum_moments_fullcommunity_without_ITV <- trait_summarise_boot_moments(Imputed_traits_fullcommunity_without_ITV)


#### Adding climate info & pivoting longer ####

sum_moments_climate_fullcommunity = bind_rows(
  sum_moments_fullcommunity  |>   
    left_join(env, by = c("siteID" = "siteID", "year" = "year")))


moments_clim_long_fullcommunity <- Imputed_traits_fullcommunity  |>   
  pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value")  |>   
  left_join(env, by = c("siteID" = "siteID", "year" = "year"))


rm(Imputed_traits_fullcommunity)
rm(sum_moments_fullcommunity)

##Without intraspecific trait variability
sum_moments_climate_fullcommunity_without_ITV = bind_rows(
  sum_moments_fullcommunity_without_ITV  |>   
    left_join(env, by = c("siteID" = "siteID", "year" = "year")))


moments_clim_long_fullcommunity_without_ITV <- Imputed_traits_fullcommunity_without_ITV  |>   
  pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value")  |>   
  left_join(env, by = c("siteID" = "siteID", "year" = "year"))


# rm(Imputed_traits_fullcommunity_without_ITV)
# rm(sum_moments_fullcommunity_without_ITV)

###### Mixed effect model testing ######

#### Making dataset for models ####

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

model_data_without_ITV <- sum_moments_climate_fullcommunity_without_ITV  |>   
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

#### Functions for models and model outputs ####


#Function for extracting phi from the model with autocorrelation
extract_phi <- function(model) {
  coef(model$modelStruct$corStruct, unconstrained = FALSE)
}

#Functions for model selection

# Define the models for climate
climate_models <- list(
  "Temp" = value ~ Temp_decade,
  "Precip" = value ~ Precip_decade,
  "Temp + Precip" = value ~ Temp_decade + Precip_decade,
  "Temp * Precip" = value ~ Temp_decade * Precip_decade
)

# Define the models for temporal
temporal_models <- list(
  "Temp + Year" = value ~ Temp_decade + year,
  "Precip + Year" = value ~ Precip_decade + year,
  "Temp + Precip + Year" = value ~ Temp_decade + Precip_decade + year,
  "Temp * Year + Precip + Year" = value ~ Temp_decade * year + Precip_decade + year,
  "Temp + Precip * Year + Year" = value ~ Temp_decade + Precip_decade * year + year,
  "Temp * Year + Precip * Year" = value ~ Temp_decade * year + Precip_decade * year,
  "Temp * Precip + Year" = value ~ Temp_decade * Precip_decade + year,
  "Temp * Precip * Year" = value ~ Temp_decade * Precip_decade * year
)

# Define the models for the without intraspecific variation - spatial
without_intraspecific_LE_spatial_model <- value ~ Precip_decade
without_intraspecific_size_spatial_model <- value ~ Temp_decade
without_intraspecific_SLA_spatial_model <- value ~ Temp_decade * Precip_decade

# Define the models for the without intraspecific variation - temporal
without_intraspecific_LE_temporal_model <- value ~ Precip_decade + year
without_intraspecific_size_temporal_model <- value ~ Temp_decade + year
without_intraspecific_SLA_temporal_model <- value ~ Temp_decade * Precip_decade + year

#Define traits
LE_traits <- c("CN_ratio", "LDMC", "Leaf_Thickness_Ave_mm", "N_percent")
size_traits <- c("Dry_Mass_g_log", "C_percent", "Leaf_Area_cm2_log", "Plant_Height_mm_log", "Wet_Mass_g_log")


# Define the hierarchy for climate models
climate_hierarchy <- list(
  "Temp" = 1,
  "Precip" = 1,
  "Temp + Precip" = 2,
  "Temp * Precip" = 3
)

# Define the hierarchy for temporal models
temporal_hierarchy <- list(
  "Temp + Year" = 1,
  "Precip + Year" = 1,
  "Temp + Precip + Year" = 2,
  "Temp * Year + Precip + Year" = 3,
  "Temp + Precip * Year + Year" = 3,
  "Temp * Year + Precip * Year" = 4,
  "Temp * Precip + Year" = 5,
  "Temp * Precip * Year" = 6
)

#Define random effects
random_effects <- as.formula("~ 1|siteID/turfID")

#Function for comparing models with AIC and rules for hierarchy of models
compare_models <- function(models, hierarchy) {
  best_model <- models[[1]]
  best_name <- names(models)[1]
  
  for (i in 2:length(models)) {
    current_model <- models[[i]]
    current_name <- names(models)[i]
    
    if (!is.null(hierarchy[[current_name]]) && !is.null(hierarchy[[best_name]])) {
      if (current_model$AIC < best_model$AIC - 2) {
        best_model <- current_model
        best_name <- current_name
      } else if (current_model$AIC < best_model$AIC && (best_model$AIC - current_model$AIC) < 2) {
        if (hierarchy[[current_name]] <= hierarchy[[best_name]]) {
          best_model <- current_model
          best_name <- current_name
        }
      }
    }
  }
  return(best_model)
}

#Function for return the same models for the without ITV data as the other data


#Fit and compare models
fit_and_compare_models <- function(df, models, random_effects, correlation = NULL, model_type, hierarchy) {
  results <- map(models, ~ {
    model <- lme(.x, random = random_effects, correlation = correlation, data = df, na.action = na.omit)
    fixed_effects <- as.character(formula(model))[3]
    list(fixed_effects = fixed_effects, model = model, AIC = AIC(model), model_type = model_type)
  })
  best_model <- compare_models(results, hierarchy)
  return(list(best_model = best_model$model, all_AICs = results))
}

#Fit without comparing models for the datasets without intraspecific

fit_and_compare_models_without_ITV <- function(df, model_formula, random_effects, correlation = NULL, model_type) {
  model <- lme(model_formula, random = random_effects, correlation = correlation, data = df, na.action = na.omit)
  
  result <- list(
    fixed_effects = as.character(formula(model))[3],
    model = model,
    AIC = AIC(model),
    model_type = model_type
  )
  
  return(list(best_model = model, all_AICs = list(result)))
}

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
    mutate(estimate = round(estimate, digits = 5)) |> 
    mutate(std.error = round(std.error, digits = 5)) |> 
    mutate(statistic = round(statistic, digits = 5)) |> 
    mutate(p.value = round(p.value, digits = 5)) |> 
    mutate(phi = round(phi, digits = 5))
  
  return(model_output)
}

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
    mutate(estimate = round(estimate, digits = 5)) |>
    mutate(std.error = round(std.error, digits = 5)) |>
    mutate(statistic = round(statistic, digits = 5)) |>
    mutate(p.value = round(p.value, digits = 5)) |>
    mutate(phi = round(phi, digits = 5)) |> 
    mutate(significant = case_when(p.value > 0.05 ~ "NO",
                                   p.value < 0.05 ~ "YES",
                                   ))
  
  return(dat3)
}


#### Running models - traits ####

models <- model_data |>  
  mutate(
    climateModelResults = map(data, ~ fit_and_compare_models(.x, climate_models, random_effects, model_type = "climate", hierarchy = climate_hierarchy)),
    temporalModelResults = map(data, ~ fit_and_compare_models(.x, temporal_models, random_effects, correlation = corAR1(form = ~ year | siteID/turfID), model_type = "temporal", hierarchy = temporal_hierarchy))
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

models_output <- output(models, unnesting = "Trait_trans")


#write.table(models_output, row.names = TRUE, col.names = TRUE, file = "model_output_traits_AfterModelSelection.csv")

## Make a table of this for the paper 

table_models_output <- flextable(models_output)

# Create a Word document and add the flextable
 doc <- read_docx() |> 
   body_add_flextable(table_models_output) |> 
   body_add_par("Model outputs", style = "heading 1")

# Save the Word document
print(doc, target = "models_output.docx")
#Don't override now (I made some pretty-changes :) )

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
#Run models for the dataset without ITV

#### Running models - traits without intraspecific variation ####

models_without_ITV <- model_data_without_ITV |>
  mutate(
    # Assign spatial (climate) model formulas based on trait
    climate_model_formula = case_when(
      Trait_trans %in% LE_traits ~ list(without_intraspecific_LE_spatial_model),
      Trait_trans %in% size_traits ~ list(without_intraspecific_size_spatial_model),
      Trait_trans == "SLA_cm2_g" ~ list(without_intraspecific_SLA_spatial_model),
      TRUE ~ NA                           # Handle unknown traits
    ),
    
    # Assign temporal model formulas
    temporal_model_formula = case_when(
      Trait_trans %in% LE_traits ~ list(without_intraspecific_LE_temporal_model),
      Trait_trans %in% size_traits ~ list(without_intraspecific_size_temporal_model),
      Trait_trans == "SLA_cm2_g"  ~ list(without_intraspecific_SLA_temporal_model),
      TRUE ~ NA
    )
  ) |>
  mutate(
    climateModelResults = map2(data, climate_model_formula, ~ fit_and_compare_models_without_ITV(.x, .y, random_effects, model_type = "climate")),
    
    temporalModelResults = map2(data, temporal_model_formula, ~ fit_and_compare_models_without_ITV(
      .x, .y, random_effects,
      correlation = corAR1(form = ~ year | siteID/turfID),
      model_type = "temporal"
    ))
  ) |>
  mutate(
    bestClimateModel = map(climateModelResults, "best_model"),
    bestTemporalModel = map(temporalModelResults, "best_model"),
    climateAICs = map(climateModelResults, ~ .x$all_AICs),
    temporalAICs = map(temporalModelResults, ~ .x$all_AICs),
    phi = map(bestTemporalModel, extract_phi),
    model_outputClimate = map(bestClimateModel, tidy),
    model_outputTemporal = map(bestTemporalModel, tidy)
  )

models_output_without_ITV <- output(models_without_ITV, unnesting = "Trait_trans")

## Make a table of this for the paper 

table_models_output <- flextable(models_output)

# Create a Word document and add the flextable
doc <- read_docx() |> 
  body_add_flextable(table_models_output) |> 
  body_add_par("Model outputs", style = "heading 1")

# Save the Word document
print(doc, target = "models_output.docx")
#Don't override now (I made some pretty-changes :) )

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
#Run models for the dataset without ITV


#### Make predictions ####

Temp_decade_vec <- seq(5.5, 12, length.out = 50)
Precip_decade_factor <- c(1.0, 1.45, 2.4, 3.5)
Precip_decade_vec <- seq(0.75, 4.5, length.out = 50)
Temp_decade_factor <- c(6.5, 8.5, 10.5)
year_vec <- seq(2008,2020, by = 1)

pred_temp <- expand.grid(Temp_decade = Temp_decade_vec, Precip_decade = mean(Precip_decade_factor))
pred_precip <- expand.grid(Precip_decade = Precip_decade_vec, Temp_decade = mean(Temp_decade_factor))
pred_interaction <- expand.grid(Precip_decade = Precip_decade_vec, Temp_decade = Temp_decade_factor)
pred_temp_year <- expand.grid(year = year_vec, Temp_decade = Temp_decade_factor, Precip_decade = mean(Precip_decade_factor))
pred_precip_year <- expand.grid(year = year_vec, Precip_decade = Precip_decade_factor, Temp_decade = mean(Temp_decade_factor))
pred_interaction_year <- expand.grid(year = year_vec, Precip_decade = Precip_decade_factor, Temp_decade = Temp_decade_factor)

temp_traits <- c("Plant_Height_mm_log", "Leaf_Area_cm2_log","Dry_Mass_g_log", "Wet_Mass_g_log", "C_percent")
precip_traits <- c("N_percent", "CN_ratio", "LDMC", "Leaf_Thickness_Ave_mm")
interaction_traits <- c("SLA_cm2_g")

pretty_trait_names <- c(
  "C_percent" = "Leaf C (%)",
  "N_percent" = "Leaf N (%)",
  "CN_ratio" = "Leaf C/N",
  "LDMC" = "LDMC",
  "Leaf_Thickness_Ave_mm" = "Leaf Thickness",
  "SLA_cm2_g" = "SLA",
  "Leaf_Area_cm2_log" = "Leaf Area",
  "Plant_Height_mm_log" = "Plant Height",
  "Wet_Mass_g_log" = "Leaf Wet Mass",
  "Dry_Mass_g_log" = "Leaf Dry Mass"
)


models_pred <- models |> 
  mutate(predicted_newdataClimate = map2(bestClimateModel, Trait_trans, ~ {
    pred <- if (.y %in% temp_traits) {
      pred_temp
    } else if (.y %in% precip_traits) {
      pred_precip
    } else if (.y %in% interaction_traits) {
      pred_interaction
    }
    pred <- pred |> mutate(
      predicted = predict(.x, newdata = pred, level = 0, se.fit = TRUE)$fit,
      se_predicted = predict(.x, newdata = pred, level = 0, se.fit = TRUE)$se.fit
    )
    pred
  })) |>
  mutate(predicted_newdataTemporal = map2(bestTemporalModel, Trait_trans, ~ {
    pred_year <- if (.y %in% temp_traits) {
      pred_temp_year
    } else if (.y %in% precip_traits) {
      pred_precip_year
    } else if (.y %in% interaction_traits) {
      pred_interaction_year
    }
    pred_year <- pred_year |> mutate(
      predicted_year = predict(.x, newdata = pred_year, level = 0, se.fit = TRUE)$fit,
      se_predicted_year = predict(.x, newdata = pred_year, level = 0, se.fit = TRUE)$se.fit
    )
    pred_year
  })) |>
  mutate(Trait_pretty = recode(Trait_trans, !!!pretty_trait_names))

## Without ITV ##

models_pred_without_ITV <- models_without_ITV |> 
  mutate(predicted_newdataClimate = map2(bestClimateModel, Trait_trans, ~ {
    pred <- if (.y %in% temp_traits) {
      pred_temp
    } else if (.y %in% precip_traits) {
      pred_precip
    } else if (.y %in% interaction_traits) {
      pred_interaction
    }
    pred <- pred |> mutate(
      predicted = predict(.x, newdata = pred, level = 0, se.fit = TRUE)$fit,
      se_predicted = predict(.x, newdata = pred, level = 0, se.fit = TRUE)$se.fit
    )
    pred
  })) |>
  mutate(predicted_newdataTemporal = map2(bestTemporalModel, Trait_trans, ~ {
    pred_year <- if (.y %in% temp_traits) {
      pred_temp_year
    } else if (.y %in% precip_traits) {
      pred_precip_year
    } else if (.y %in% interaction_traits) {
      pred_interaction_year
    }
    pred_year <- pred_year |> mutate(
      predicted_year = predict(.x, newdata = pred_year, level = 0, se.fit = TRUE)$fit,
      se_predicted_year = predict(.x, newdata = pred_year, level = 0, se.fit = TRUE)$se.fit
    )
    pred_year
  })) |>
  mutate(Trait_pretty = recode(Trait_trans, !!!pretty_trait_names))
  

#Extract predictions for vizualization
predictions_climate <- models_pred |> 
  select(Trait_trans, predicted_newdataClimate) |> 
  unnest(predicted_newdataClimate)

predictions_temporal <- models_pred |> 
  select(Trait_trans, predicted_newdataTemporal) |> 
  unnest(predicted_newdataTemporal)

predictions_climate_without_ITV <- models_pred_without_ITV |> 
  select(Trait_trans, predicted_newdataClimate) |> 
  unnest(predicted_newdataClimate)

predictions_temporal_without_ITV <- models_pred_without_ITV |> 
  select(Trait_trans, predicted_newdataTemporal) |> 
  unnest(predicted_newdataTemporal)


#### Compare spatial and temporal models ####

## Climate effect size over time

temp_year_effect <- tidy(temp_model) |> 
  filter(term == "year") |> 
  pull(estimate)

precip_year_effect <- tidy(precip_model) |> 
  filter(term == "year") |> 
  pull(estimate)

## Traits

temp_traits 
precip_traits 
interaction_traits 



## Temperature traits

temp_spatial_effect <- models_output |> 
  filter(Trait_trans %in% c(temp_traits, "SLA_cm2_g"),
         term == "Temp_decade",
         model == "climate") |> 
  select(Trait_trans, term, estimate, p.value, significant) |> 
  mutate(trend = "temp_spatial",
         data = "with ITV",
         trend2 = "temp_spatial_withITV")

temp_spatial_effect_without_ITV <- models_output_without_ITV |> 
  filter(Trait_trans %in% c(temp_traits, "SLA_cm2_g"),
         term == "Temp_decade",
         model == "climate") |> 
  select(Trait_trans, term, estimate, p.value, significant) |> 
  mutate(trend = "temp_spatial",
         data = "without ITV",
         trend2 = "temp_spatial_withoutITV")

temp_temporal_effect <- models_output |> 
  filter(Trait_trans %in% c(temp_traits, "SLA_cm2_g"),
         term == "year",
         model == "year") |> 
  select(Trait_trans, term, estimate, p.value, significant) |> 
  mutate(estimate = estimate/temp_year_effect) |> 
  mutate(term = "Temp_decade",
         trend = "temp_temporal",
         data = "with ITV",
         trend2 = "temp_temporal_withITV")

temp_temporal_effect_without_ITV <- models_output_without_ITV |> 
  filter(Trait_trans %in% c(temp_traits, "SLA_cm2_g"),
         term == "year",
         model == "year") |> 
  select(Trait_trans, term, estimate, p.value, significant) |> 
  mutate(estimate = estimate / temp_year_effect) |> 
  mutate(term = "Temp_decade", 
         trend = "temp_temporal",
         data = "without ITV",
         trend2 = "temp_temporal_withoutITV")



temp_trait_spatialANDtemporal <- temp_spatial_effect |> 
  bind_rows(temp_temporal_effect)

temp_trait_spatialANDtemporal_and_ITV <- temp_spatial_effect |> 
  bind_rows(temp_spatial_effect_without_ITV) |> 
  bind_rows(temp_temporal_effect) |> 
  bind_rows(temp_temporal_effect_without_ITV)
  


## Precipitation traits

precip_spatial_effect <- models_output |> 
  filter(Trait_trans %in% c(precip_traits, "SLA_cm2_g"),
         term == "Precip_decade",
         model == "climate") |> 
  select(Trait_trans, term, estimate, p.value, significant) |> 
  mutate(trend = "precip_spatial",
         data = "with ITV",
         trend2 = "precip_spatial_withITV")

precip_spatial_effect_without_ITV <- models_output_without_ITV |> 
  filter(Trait_trans %in% c(precip_traits, "SLA_cm2_g"),
         term == "Precip_decade",
         model == "climate") |> 
  select(Trait_trans, term, estimate, p.value, significant) |> 
  mutate(trend = "precip_spatial",
         data = "without ITV",
         trend2 = "precip_spatial_withoutITV")

precip_temporal_effect <- models_output |> 
  filter(Trait_trans %in% c(precip_traits, "SLA_cm2_g"),
         term == "year",
         model == "year") |> 
  select(Trait_trans, term, estimate, p.value, significant) |> 
  mutate(estimate = estimate/precip_year_effect) |> 
  mutate(term = "Precip_decade",
         trend = "precip_temporal",
         data = "with ITV",
         trend2 = "precip_temporal_withITV")

precip_temporal_effect_without_ITV <- models_output_without_ITV |> 
  filter(Trait_trans %in% c(precip_traits, "SLA_cm2_g"),
         term == "year",
         model == "year") |> 
  select(Trait_trans, term, estimate, p.value, significant) |> 
  mutate(estimate = estimate/precip_year_effect) |> 
  mutate(term = "Precip_decade",
         trend = "precip_temporal",
         data = "without ITV",
         trend2 = "precip_temporal_withoutITV")


precip_trait_spatialANDtemporal <- precip_spatial_effect |> 
  bind_rows(precip_temporal_effect)

precip_trait_spatialANDtemporal_and_ITV <- precip_spatial_effect |> 
  bind_rows(precip_spatial_effect_without_ITV) |> 
  bind_rows(precip_temporal_effect) |> 
  bind_rows(precip_temporal_effect_without_ITV)



SpatialTemporal_comparison <- temp_trait_spatialANDtemporal |> 
  bind_rows(precip_trait_spatialANDtemporal)

SpatialTemporal_comparison_and_ITV <- temp_trait_spatialANDtemporal_and_ITV |> 
  bind_rows(precip_trait_spatialANDtemporal_and_ITV)

## Interaction - SLA

SLA_interaction_effect <- models_output |> 
  filter(Trait_trans == "SLA_cm2_g",
         model == "climate") |> 
  select(Trait_trans, term, estimate, p.value, significant, model) 

SLA_year_effect <- models_output |> 
  filter(Trait_trans == "SLA_cm2_g",
         model == "year",
         term == "year") |> 
  select(Trait_trans, term, estimate, p.value, significant, model) 


# Extract coefficients
b_temp <- SLA_interaction_effect %>% filter(term == "Temp_decade") %>% pull(estimate)
b_precip <- SLA_interaction_effect %>% filter(term == "Precip_decade") %>% pull(estimate)
b_interaction <- SLA_interaction_effect %>% filter(term == "Temp_decade:Precip_decade") %>% pull(estimate)
year_effect <- SLA_year_effect %>% pull(estimate)

# Climate grid
temp_levels <- c(6.5, 8.5, 10.5)
precip_levels <- c(1.0, 1.45, 2.4, 3.5)

# Spatial effects
temp_effects <- b_temp + b_interaction * precip_levels
names(temp_effects) <- paste0("Temperature trend at: ", precip_levels, " m/year")

precip_effects <- b_precip + b_interaction * temp_levels
names(precip_effects) <- paste0("Precipitation trend at: ", temp_levels, " °C")

# Temporal effects (translated)
temp_effect_temporal <- year_effect / temp_year_effect
precip_effect_temporal <- year_effect / precip_year_effect

# Assemble for plotting
spatial_temp_df <- tibble(
  variable = "Temperature",
  level = names(temp_effects),
  effect = as.numeric(temp_effects),
  source = "Spatial",
  data = "with ITV"
)

spatial_precip_df <- tibble(
  variable = "Precipitation",
  level = names(precip_effects),
  effect = as.numeric(precip_effects),
  source = "Spatial",
  data = "with ITV"
)

temporal_df <- tibble(
  variable = c("Temperature", "Precipitation"),
  level = c("Temperature (temporal)", "Precipitation (temporal)"),
  effect = c(temp_effect_temporal, precip_effect_temporal),
  source = "Temporal",
  data = "with ITV"
) 

effects_df <- bind_rows(spatial_temp_df, spatial_precip_df, temporal_df) 

effects_df <- effects_df |> 
  mutate(significant = case_when(source == "Spatial" ~ "YES",
                                 source == "Temporal" ~ "NO"))

## SLA without intraspecific ##

SLA_interaction_effect_without_ITV <- models_output_without_ITV |> 
  filter(Trait_trans == "SLA_cm2_g", model == "climate") |> 
  select(Trait_trans, term, estimate, p.value, significant)

SLA_year_effect_without_ITV <- models_output_without_ITV |> 
  filter(Trait_trans == "SLA_cm2_g", model == "year", term == "year") |> 
  pull(estimate)

# Extract coefficients
b_temp_wo <- SLA_interaction_effect_without_ITV %>% filter(term == "Temp_decade") %>% pull(estimate)
b_precip_wo <- SLA_interaction_effect_without_ITV %>% filter(term == "Precip_decade") %>% pull(estimate)
b_interaction_wo <- SLA_interaction_effect_without_ITV %>% filter(term == "Temp_decade:Precip_decade") %>% pull(estimate)

# Spatial effects
temp_effects_wo <- b_temp_wo + b_interaction_wo * precip_levels
names(temp_effects_wo) <- paste0("Temperature trend at: ", precip_levels, " m/year")

precip_effects_wo <- b_precip_wo + b_interaction_wo * temp_levels
names(precip_effects_wo) <- paste0("Precipitation trend at: ", temp_levels, " °C")


# Temporal effects (translated)
# temp_effect_temporal_wo <- SLA_year_effect_without_ITV / temp_year_effect
# precip_effect_temporal_wo <- SLA_year_effect_without_ITV / precip_year_effect


# Assemble for plotting (no ITV)
spatial_temp_df_wo <- tibble(
  variable = "Temperature",
  level = names(temp_effects_wo),
  effect = as.numeric(temp_effects_wo),
  source = "Spatial",
  data = "without ITV"
)

spatial_precip_df_wo <- tibble(
  variable = "Precipitation",
  level = names(precip_effects_wo),
  effect = as.numeric(precip_effects_wo),
  source = "Spatial",
  data = "without ITV"
)

# temporal_df_wo <- tibble(
#   variable = c("Temperature", "Precipitation"),
#   level = c("Temperature (temporal)", "Precipitation (temporal)"),
#   effect = c(temp_effect_temporal_wo, precip_effect_temporal_wo),
#   source = "Temporal",
#   data = "without ITV"
# ) 

effects_df_wo <- bind_rows(spatial_temp_df_wo, spatial_precip_df_wo, temporal_df_wo)

effects_df_wo <- effects_df_wo |> 
  mutate(significant = case_when(source == "Spatial" ~ "YES",
                                 source == "Temporal" ~ "NO"))

effects_df_full <- effects_df |> 
  bind_rows(effects_df_wo)

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
RsquareAdj(RDA_year)$adj.r.squared
RsquareAdj(RDA_temp)$adj.r.squared
RsquareAdj(RDA_precip)$adj.r.squared
RsquareAdj(RDA_space_additive)$adj.r.squared
RsquareAdj(RDA_space)$adj.r.squared
RsquareAdj(RDA_add_year)$adj.r.squared
RsquareAdj(RDA_multi_year)$adj.r.squared

