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

#### Making data ready for traitstrap ####

community2009 <- community %>% 
  filter(year == "2009")

community2011 <- community %>% 
  filter(year == "2011")

community2012 <- community %>% 
  filter(year == "2012")

community2013 <- community %>% 
  filter(year == "2013")

community2015 <- community %>% 
  filter(year == "2015")

community2016 <- community %>% 
  filter(year == "2016")

community2017 <- community %>% 
  filter(year == "2017")

traitdata_2 <- traitdata_1 %>% 
  mutate(blockID = "",
         turfID = "")

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
                                       abundance_col = "cover")
  
  return(SeedClim_traits)
}

SeedClim_traits_allYears <- Trait_impute_per_year(com_dat = community, trait_dat = traitdata_2)
SeedClim_traits_2009 <- Trait_impute_per_year(com_dat = community2009, trait_dat = traitdata_2)
SeedClim_traits_2011 <- Trait_impute_per_year(com_dat = community2011, trait_dat = traitdata_2)
SeedClim_traits_2012 <- Trait_impute_per_year(com_dat = community2012, trait_dat = traitdata_2)
SeedClim_traits_2013 <- Trait_impute_per_year(com_dat = community2013, trait_dat = traitdata_2)
SeedClim_traits_2015 <- Trait_impute_per_year(com_dat = community2015, trait_dat = traitdata_2)
SeedClim_traits_2016 <- Trait_impute_per_year(com_dat = community2016, trait_dat = traitdata_2)
SeedClim_traits_2017 <- Trait_impute_per_year(com_dat = community2017, trait_dat = traitdata_2)


#### Bootstraping community weighted means and making summarised moments of the distributions ####

SC_moments_2009 <- trait_np_bootstrap(imputed_traits = SeedClim_traits_2009, nrep = 100)
SC_moments_2011 <- trait_np_bootstrap(imputed_traits = SeedClim_traits_2011, nrep = 100)
SC_moments_2012 <- trait_np_bootstrap(imputed_traits = SeedClim_traits_2012, nrep = 100)
SC_moments_2013 <- trait_np_bootstrap(imputed_traits = SeedClim_traits_2013, nrep = 100)
SC_moments_2015 <- trait_np_bootstrap(imputed_traits = SeedClim_traits_2015, nrep = 100)
SC_moments_2016 <- trait_np_bootstrap(imputed_traits = SeedClim_traits_2016, nrep = 100)
SC_moments_2017 <- trait_np_bootstrap(imputed_traits = SeedClim_traits_2017, nrep = 100)

sum_SC_moments_2009 <- trait_summarise_boot_moments(SC_moments_2009)
sum_SC_moments_2011 <- trait_summarise_boot_moments(SC_moments_2011)
sum_SC_moments_2012 <- trait_summarise_boot_moments(SC_moments_2012)
sum_SC_moments_2013 <- trait_summarise_boot_moments(SC_moments_2013)
sum_SC_moments_2015 <- trait_summarise_boot_moments(SC_moments_2015)
sum_SC_moments_2016 <- trait_summarise_boot_moments(SC_moments_2016)
sum_SC_moments_2017 <- trait_summarise_boot_moments(SC_moments_2017)


#### Adding climate info & pivoting longer ####

summarised_boot_moments_climate_2009 = bind_rows(
  sum_SC_moments_2009 %>% 
    left_join(env, by = c("Site" = "Site")))

summarised_boot_moments_climate_2011 = bind_rows(
  sum_SC_moments_2011 %>% 
    left_join(env, by = c("Site" = "Site")))

summarised_boot_moments_climate_2012 = bind_rows(
  sum_SC_moments_2012 %>% 
    left_join(env, by = c("Site" = "Site")))

summarised_boot_moments_climate_2013 = bind_rows(
  sum_SC_moments_2013 %>% 
    left_join(env, by = c("Site" = "Site")))

summarised_boot_moments_climate_2015 = bind_rows(
  sum_SC_moments_2015 %>% 
    left_join(env, by = c("Site" = "Site")))

summarised_boot_moments_climate_2016 = bind_rows(
  sum_SC_moments_2016 %>% 
    left_join(env, by = c("Site" = "Site")))

summarised_boot_moments_climate_2017 = bind_rows(
  sum_SC_moments_2017 %>% 
    left_join(env, by = c("Site" = "Site")))


Moments_env_long <- function(moments, env){
  
  SC_moments_clim_long = bind_rows(
    moments %>% 
    pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value"))
  
  return(SC_moments_clim_long)
}

SC_moments_2009_clim_long <- Moments_env_long(SC_moments_2009, env)
SC_moments_2011_clim_long <- Moments_env_long(SC_moments_2011, env)
SC_moments_2012_clim_long <- Moments_env_long(SC_moments_2012, env)
SC_moments_2013_clim_long <- Moments_env_long(SC_moments_2013, env)
SC_moments_2015_clim_long <- Moments_env_long(SC_moments_2015, env)
SC_moments_2016_clim_long <- Moments_env_long(SC_moments_2016, env)
SC_moments_2017_clim_long <- Moments_env_long(SC_moments_2017, env)


#### Adding all the years together ####

dict_year <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
                          "old new
  value2009 2009
  value2011 2011
  value2012 2012
  value2013 2013
  value2015 2015
  value2016 2016
  value2017 2017")

SC_moments_clim_allYears <- SC_moments_2009_clim_long %>% 
  left_join(SC_moments_2011_clim_long, by = c("n", "Site", "blockID", "turfID", "Trait_trans", "moments"), suffix = c("2009", "2011")) %>% 
  left_join(SC_moments_2012_clim_long, by = c("n", "Site", "blockID", "turfID", "Trait_trans", "moments")) %>% 
  left_join(SC_moments_2013_clim_long, by = c("n", "Site", "blockID", "turfID", "Trait_trans", "moments"), suffix = c("2012", "2013")) %>% 
  left_join(SC_moments_2015_clim_long, by = c("n", "Site", "blockID", "turfID", "Trait_trans", "moments")) %>% 
  left_join(SC_moments_2016_clim_long, by = c("n", "Site", "blockID", "turfID", "Trait_trans", "moments"), suffix = c("2015", "2016")) %>% 
  left_join(SC_moments_2017_clim_long, by = c("n", "Site", "blockID", "turfID", "Trait_trans", "moments")) %>% 
  mutate(value2017 = value) %>% 
  select(-value) %>% 
  left_join(env, by = c("Site" = "Site")) %>% 
  pivot_longer(c("value2009", "value2011", "value2012", "value2013", "value2015", "value2016", "value2017"), names_to = "year", values_to = "value") %>% 
  mutate(year = plyr::mapvalues(year, from = dict_year$old, to = dict_year$new)) %>% 
  mutate(year = as.numeric(year))
  


#### Mixed effect model testing ####

mixed_model_temp_precip<-function(df) {
  lmer(value ~ Temp * scale(Precip) + (1 | Site), data = df)
}

mixed_model_clim_year<-function(df) {
  lmer(value ~ year + (1 | Site), data = df)
}

predict_without_random<-function(model) {
  predict(object = model, re.form=NA)
}

predict_with_random<-function(model) {
  predict(object = model, re.form=NULL)
}
# Making  dataset ready for model 

memodel_data <-function(dat) {
  dat2 <- dat %>% 
    select(Trait_trans, moments, Site, blockID, turfID, Temp, Precip, value) %>% 
    group_by(Trait_trans, moments, turfID) %>% 
    mutate(n = 1:n()) %>% 
    ungroup() %>%
    select(-turfID) %>% 
    group_by(Trait_trans, moments, n) %>% 
    nest()
  return(dat2)
}

me_year_model_data <-function(dat) {
  dat2 <- dat %>% 
    ungroup() %>% 
    select(Trait_trans, moments, Site, turfID, n, Temp, Precip, value, year) %>% 
    # group_by(Trait_trans, moments, turfID) %>% 
    # mutate(n = 1:n()) %>% 
    # select(-turfID) %>% 
    group_by(Trait_trans, moments, n) %>% 
    nest()
  return(dat2)
}

me_year_model_data <- me_year_model_data(dat = SC_moments_clim_allYears) 

memodel_data_2009 <- memodel_data(dat = SC_moments_2009_clim_long) 
memodel_data_2011 <- memodel_data(dat = SC_moments_2011_clim_long)
memodel_data_2012 <- memodel_data(dat = SC_moments_2012_clim_long) 
memodel_data_2013 <- memodel_data(dat = SC_moments_2013_clim_long)
memodel_data_2015 <- memodel_data(dat = SC_moments_2015_clim_long) 
memodel_data_2016 <- memodel_data(dat = SC_moments_2016_clim_long) 
memodel_data_2017 <- memodel_data(dat = SC_moments_2017_clim_long) 

#### Testing with mixed effect model - TIME CONSUMING ####

mem_results_clim_year <- me_year_model_data %>%
  mutate(model = map(data, mixed_model_clim_year))

mem_results_2009 <- memodel_data_2009 %>%
  mutate(model = map(data, mixed_model_temp_precip))
# mem_results_2011 <- memodel_data_2011 %>%
#   mutate(model = map(data, mixed_model_temp_precip))
# mem_results_2012 <- memodel_data_2012 %>%
#   mutate(model = map(data, mixed_model_temp_precip))
mem_results_2013 <- memodel_data_2013 %>%
  mutate(model = map(data, mixed_model_temp_precip))
# mem_results_2015 <- memodel_data_2015 %>%
#   mutate(model = map(data, mixed_model_temp_precip))
# mem_results_2016 <- memodel_data_2016 %>%
#   mutate(model = map(data, mixed_model_temp_precip))
mem_results_2017 <- memodel_data_2017 %>%
  mutate(model = map(data, mixed_model_temp_precip))

# Tidying the model, giving the model output in a nice format. Making predicted values for each of the trait:moment combination along the climatic gradients. Calculating pseudo R squared values based on the method in paper Nakagawa et al 2017.

tidy_year_model_predicted <- mem_results_clim_year %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_with_random)) %>% 
  mutate(R_squared = map(model, rsquared))

tidy_model_predicted_09 <- mem_results_2009 %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_without_random)) %>% 
  mutate(R_squared = map(model, rsquared))

tidy_model_predicted_13 <- mem_results_2013 %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_without_random)) %>% 
  mutate(R_squared = map(model, rsquared))

tidy_model_predicted_17 <- mem_results_2017 %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_without_random)) %>% 
  mutate(R_squared = map(model, rsquared))

# Making a dataset with only the predicted values. Unnesting the list of the predicted values.

predicted_values_allyear <- tidy_year_model_predicted %>%
  #filter(!(Trait_trans == "C_percent" & moments == "kurtosis" & n == 79)) %>% 
  #filter(!(Trait_trans == "Leaf_area_cm2_log" & moments == "variance" & n == 23)) %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)

predicted_values_09 <- tidy_model_predicted_09 %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)

predicted_values_13 <- tidy_model_predicted_13 %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)

predicted_values_17 <- tidy_model_predicted_17 %>%
  ungroup() %>%
  select(Trait_trans, moments, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = value)

# Making a dataset with the model output and the test-statistics (R squared), and summarizing them.

model_output <-function(dat) {
  
  model_output <- dat %>% 
    select(Trait_trans, moments, n, model_output, R_squared) %>% 
    unnest(c(model_output, R_squared)) %>% 
    filter(term %in% c("Temp", "scale(Precip)", "Temp:scale(Precip)")) %>% 
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

model_output_09 <- model_output(tidy_model_predicted_09)
model_output_13 <- model_output(tidy_model_predicted_13)
model_output_17 <- model_output(tidy_model_predicted_17)

write.table(x = model_output_09, file = "model_output_09.csv")
write.table(x = model_output_13, file = "model_output_13.csv") 
write.table(x = model_output_17, file = "model_output_17.csv") 


#### Correlation ####

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
Ord_boot_traits_2009 <- summarised_boot_moments_climate_2009 %>% 
  ungroup() %>% 
  filter(!Trait_trans == "Wet_Mass_g_log") %>% 
  select(turfID, Trait_trans, T_level, P_level, mean) %>% 
  #gather(Moment, Value, -(turfID:P_cat)) %>% 
  #unite(temp, Trait, Moment) %>% 
  spread(key = Trait_trans, value = mean) %>% 
  column_to_rownames("turfID")

res.pca_09 <- prcomp(Ord_boot_traits_2009[, -(1:2)], scale = TRUE)

fviz_eig(res.pca_09, addlabels = TRUE) #Visualize eigenvalues/scree plot

### Constrained ordination ###

RDA <- rda(Ord_boot_traits[, -(1:3)]~ Temp+Precip, scale = TRUE, data = Ord_boot_traits)

autoplot(RDA) +
  theme_bw()

autoplot(RDA, arrows = TRUE, data = PCA_boot_traits) +
  scale_x_continuous(expand = c(0.22, 0)) +
  geom_point(data = RDA, aes(RDA1, RDA2), size=2) +
  geom_abline(intercept = 0,slope = 0,linetype="dashed", size=0.8) +
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8) + labs(x = "Axis 1", y="Axis 2") + 
  theme_bw()

RDA_fort <- fortify(RDA)

ggplot(RDA_fort, aes(x = RDA1, y = RDA2)) +
  geom_point(show.legend = FALSE) +
  scale_size(range = 2) +
  coord_equal()


RDA_VPD
plot(RDA)
screeplot(RDA)

coef(RDA)

RsquareAdj(RDA)$adj.r.squared