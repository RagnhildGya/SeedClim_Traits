#### Run analysis ####

#### Source ####

source("R/Cleaning.R")
#source("R/Bootstraping.R")

#### Libraries ####

 library(broom.mixed)
 library(lme4)
# library(lmerTest)
 library(purrr)
 library(piecewiseSEM)
 library(factoextra)
# library(GGally)
 library(ggcorrplot)
# library(textshape)
library(traitstrap)
library(vegan)
library(ggvegan)
library(drake)

set.seed(47)

#### Making data ready for traitstrap and merging ####

community <- community %>% 
  filter(!year %in% c("2010", "2016"))

traitdata_2 <- traitdata_1 %>% 
  mutate(blockID = "",
         turfID = "") 

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

##Fix species with missing taxonomy
##Check that Car_sp, Alc_sp have a taxonomy

# Trait_impute_per_year <- function(com_dat, trait_dat){
#   
#   SeedClim_traits <- trait_impute(comm = com_dat,
#                                   traits = trait_dat, 
#                                   scale_hierarchy = c("Site", "blockID", "turfID"),
#                                   global = FALSE,
#                                   taxon_col = c("Full_name", "Genus", "Family"),
#                                   trait_col = "Trait_trans",
#                                   value_col = "Value",
#                                   abundance_col = "cover")
#   
#   return(SeedClim_traits)
# }
# SeedClim_traits_allYears <- Trait_impute_per_year(com_dat = community, trait_dat = traitdata_2)
#   #group_by(year, Site, blockID, turfID, Trait_trans)


SeedClim_traits_allYears <- trait_impute(comm = community,
             traits = traitdata_2, 
             scale_hierarchy = c("Site", "blockID", "turfID"),
             global = FALSE,
             taxon_col = c("Full_name", "Genus", "Family"),
             trait_col = "Trait_trans",
             value_col = "Value",
             other_col = "year",
             abundance_col = "cover") %>% 
  group_by(year, Site, blockID, turfID, Trait_trans)



#### Bootstraping community weighted means and making summarised moments of the distributions ####

SC_moments_allYears <- trait_np_bootstrap(imputed_traits = SeedClim_traits_allYears, nrep = 100)

sum_SC_moments_allYears <- trait_summarise_boot_moments(SC_moments_allYears)


#### Adding climate info & pivoting longer ####

summarised_boot_moments_climate = bind_rows(
   sum_SC_moments_allYears %>% 
     left_join(env, by = c("Site" = "Site", "year" = "Year")))

SC_moments_clim_long <- SC_moments_allYears %>% 
  pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value") %>% 
  left_join(env, by = c("Site" = "Site", "year" = "Year"))


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
memodel_data_allYears <- SC_moments_clim_long %>% 
  filter(Trait_trans %in% c("CN_ratio_log", "Leaf_Area_cm2_log", "Plant_Height_mm_log", "SLA_cm2_g_log")) %>% 
  ungroup() %>%
  select(Trait_trans, moments, Site, turfID, Temp_yearly, Precip_yearly, Temp_deviation_decade, Precip_deviation_decade, value, year, n) %>% 
  group_by(Trait_trans, moments, n) %>% 
  nest()

### Funcitons for different models and model predictions later ###

model_null <-function(df) {
  lmer(value ~ (1 | Site/turfID), data = df)
}

model_temp <-function(df) {
  lmer(value ~ Temp_yearly + Temp_deviation_decade + (1 | Site/turfID), data = df)
}

model_precip <-function(df) {
  lmer(value ~ Precip_yearly + Precip_deviation_decade + (1 | Site/turfID), data = df)
}

model_tempAddprecip<-function(df) {
  lmer(value ~Temp_yearly + Temp_deviation_decade + scale(Precip_yearly) + scale(Precip_deviation_decade) + (1 | Site/turfID), data = df)
}

model_tempMulprecip<-function(df) {
  lmer(value ~Temp_yearly * scale(Precip_yearly) + Temp_deviation_decade*scale(Precip_deviation_decade) + (1 | Site/turfID), data = df)
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

## Null model ##
mem_results_null <- memodel_data_allYears%>%
  mutate(model = map(data, model_null))

tidy_null_model_predicted <- mem_results_null %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_null <- tidy_null_model_predicted %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)



## Temp ##
mem_results_temp <- memodel_data_allYears%>%
  mutate(model = map(data, model_temp))

tidy_temp_model_predicted <- mem_results_temp %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_temp <- tidy_temp_model_predicted %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)


## Precipitation ##
mem_results_precip <- memodel_data_allYears%>%
  mutate(model = map(data, model_precip))

tidy_precip_model_predicted <- mem_results_precip %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_precip <- tidy_precip_model_predicted %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)

## Temperature + Precipitation (additive) ##
mem_results_tempAddprecip <- memodel_data_allYears%>%
  mutate(model = map(data, model_tempAddprecip))

tidy_tempAddprecip_model_predicted <- mem_results_tempAddprecip %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_tempAddprecip <- tidy_tempAddprecip_model_predicted %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)

## Temperature * Precipitation (multiplicative) ##
mem_results_tempMulprecip <- memodel_data_allYears%>%
  mutate(model = map(data, model_tempMulprecip))

tidy_tempMulprecip_model_predicted <- mem_results_tempMulprecip %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

predicted_values_tempMulprecip <- tidy_tempMulprecip_model_predicted %>%
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

# Get AIC for all models and compare which is beter

AIC_null <- AIC_models(tidy_null_model_predicted, "Temp") %>% rename(Null_AIC = AIC)
AIC_temp <- AIC_models(tidy_temp_model_predicted, "Temp") %>% rename(Temp_AIC = AIC)
AIC_precip <- AIC_models(tidy_precip_model_predicted, "Precip") %>% rename(Precip_AIC = AIC)
AIC_tempAddprecip <- AIC_models(tidy_tempAddprecip_model_predicted, "Precip") %>% rename(TempAddPrecip_AIC = AIC)
AIC_tempMulprecip <- AIC_models(tidy_tempMulprecip_model_predicted, "Precip") %>% rename(TempMulPrecip_AIC = AIC)

AIC_all_models <- AIC_null %>%
  left_join(AIC_temp, by = c("Trait_trans" = "Trait_trans", "moments" = "moments")) %>% 
  left_join(AIC_precip, by = c("Trait_trans" = "Trait_trans", "moments" = "moments")) %>% 
  left_join(AIC_tempAddprecip, by = c("Trait_trans" = "Trait_trans", "moments" = "moments")) %>% 
  left_join(AIC_tempMulprecip, by = c("Trait_trans" = "Trait_trans", "moments" = "moments"))
  
write.table(x = AIC_all_models, file = "AIC_log.csv")

# Making a dataset with the model output and the test-statistics (R squared), and summarizing them.
model_output <-function(dat) {
  
  model_output <- dat %>% 
    select(Trait_trans, moments, n, model_output, R_squared) %>% 
    filter(Trait_trans %in% c("CN_ratio", "SLA_cm2_g") & moments %in% c("mean", "variance")) %>% 
    filter(!Trait_trans == "CN_ratio" | moments == "variance") %>% 
    unnest(c(model_output, R_squared)) %>% 
    filter(term %in% c("Temp_yearly", "Temp_deviation_decade","scale(Precip_yearly)", "scale(Precip_deviation_decade)", "Temp_yearly:scale(Precip_yearly)", "Temp_deviation_decade:scale(Precip_deviation_decade)")) %>% 
    select(Trait_trans, moments, term, n, estimate, Marginal, Conditional) %>% 
    ungroup() %>% 
    group_by(Trait_trans, moments, term) %>% 
    summarize(effect = mean(estimate),
              R2_marginal = mean(Marginal),
              R2_conditional = mean(Conditional),
              CIlow.fit = effect - sd(estimate),
              CIhigh.fit = effect + sd(estimate)) %>% 
    mutate(Significant = case_when(CIlow.fit < 0 & CIhigh.fit < 0 ~ "Negative",
                                   CIlow.fit > 0 & CIhigh.fit > 0 ~ "Positive",
                                   CIlow.fit < 0 & CIhigh.fit > 0 ~ "No"))
  return(model_output)
}

#model_output_temp <- model_output(tidy_temp_model_predicted, c("Temp_yearly", "Temp_deviation_decade"))
#model_output_precip <- model_output(tidy_temp_model_predicted, c("Precip_yearly", "Precip_deviation_decade"))
#model_output_tempAddprecip <- model_output(tidy_tempAddprecip_model_predicted, c("Temp_yearly", "Temp_deviation_decade","scale(Precip_yearly)", "scale(Precip_deviation_decade)"))
model_output_tempMulprecip <- model_output(tidy_tempMulprecip_model_predicted)

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

Ord_boot_traits <- SC_moments_allYears %>% 
  left_join(env, by = c("Site" = "Site", "year" = "Year")) %>% 
  ungroup() %>% 
  mutate(uniqueID = paste0(turfID,"_", year, "_", Site)) %>% 
  group_by(uniqueID, Trait_trans) %>% 
  mutate(mean_mean = mean(mean)) %>% 
  filter(!Trait_trans == "Wet_Mass_g_log") %>% 
  select(uniqueID, Site, year, turfID, Trait_trans, Temp_level, Precip_level, mean_mean) %>%
  unique() %>% 
  #gather(Moment, Value, -(turfID:P_cat)) %>% 
  #unite(temp, Trait, Moment) %>% 
  pivot_wider(names_from = Trait_trans, values_from = mean_mean) %>% 
  column_to_rownames("uniqueID")

Ord_boot_LeafEconomic <- SC_moments_allYears %>% 
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

Ord_boot_Size <- SC_moments_allYears %>% 
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
pca_trait <- prcomp(Ord_boot_traits[, -(1:5)], scale = TRUE)
pca_leaf_economic <- prcomp(Ord_boot_LeafEconomic[, -(1:5)], scale = TRUE)
pca_size <- prcomp(Ord_boot_Size[, -(1:5)], scale = TRUE)

### Get variable
var <- get_pca_var(pca_trait)

### Get results
pca_trait_results <- get_pca_ind(pca_trait)
pca_leaf_economic_results <- get_pca_ind(pca_leaf_economic)
pca_size_results <- get_pca_ind(pca_size)

### Pull out axis values for different ordinations
Ord_site_env_dat <- SC_moments_allYears %>% 
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