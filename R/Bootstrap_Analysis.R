#### Run analysis ####

#### Source ####

source("R/Cleaning.R")
#source("R/Bootstraping.R")

#### Libraries ####

 library(broom.mixed)
 library(lme4)
# library(lmerTest)
# library(purrr)
 library(piecewiseSEM)
 library(factoextra)
# library(GGally)
 library(ggcorrplot)
# library(textshape)
library(traitstrap)
library(vegan)
#library(ggvegan)

set.seed(47)

#### Making data ready for traitstrap ####

community2009 <- community %>% 
  filter(!is.na(cover9))

community2017 <- community %>% 
  filter(!is.na(cover17))

traitdata_2 <- traitdata_1 %>% 
  mutate(blockID = "",
         turfID = "")

#### Trait impute ####

SeedClim_traits_2009 <- trait_impute(comm = community2009,
                                traits = traitdata_2, 
                                scale_hierarchy = c("Site", "blockID", "turfID"),
                                global = FALSE,
                                taxon_col = c("Full_name", "Genus", "Family"),
                                trait_col = "Trait_trans",
                                value_col = "Value",
                                abundance_col = "cover9")

SeedClim_traits_2017 <- trait_impute(comm = community2017,
                                     traits = traitdata_2, 
                                     scale_hierarchy = c("Site", "blockID", "turfID"),
                                     global = FALSE,
                                     taxon_col = c("Full_name", "Genus", "Family"),
                                     trait_col = "Trait_trans",
                                     value_col = "Value",
                                     abundance_col = "cover17")

#### Bootstraping community weighted means and making summarised moments of the distributions ####

SC_moments_2009 <- trait_np_bootstrap(imputed_traits = SeedClim_traits_2009, nrep = 100)
SC_moments_2017 <- trait_np_bootstrap(imputed_traits = SeedClim_traits_2017, nrep = 100)

sum_SC_moments_2009 = trait_summarise_boot_moments(SC_moments_2009)
sum_SC_moments_2017 = trait_summarise_boot_moments(SC_moments_2017)


#### Adding climate info & pivoting longer ####

SC_moments_2009_clim_long = bind_rows(
  SC_moments_2009 %>% 
    left_join(env, by = c("Site" = "Site"))) %>% 
  pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value")

summarised_boot_moments_climate_2009 = bind_rows(
  sum_SC_moments_2009 %>% 
    left_join(env, by = c("Site" = "Site")))


SC_moments_2017_clim_long = bind_rows(
  SC_moments_2017 %>% 
    left_join(env, by = c("Site" = "Site"))) %>% 
  pivot_longer(c("mean", "variance", "skewness", "kurtosis"), names_to = "moments", values_to = "value")

summarised_boot_moments_climate_2017 = bind_rows(
  sum_SC_moments_2017 %>% 
    left_join(env, by = c("Site" = "Site")))

#### Mixed effect model testing ####

mixed_model_temp_precip<-function(df) {
  lmer(value ~ Temp * scale(Precip) + (1 | Site), data = df)
}

predict_without_random<-function(model) {
  predict(object = model, re.form=NA)
}


# Making  dataset ready for model 

SC_moments_2009_clim_long <- SC_moments_2009_clim_long %>% 
  select(Trait_trans, moments, Site, turfID, Temp, Precip, value) %>% 
  group_by(Trait_trans, moments, turfID) %>% 
  mutate(n = 1:n()) %>% 
  ungroup() %>%
  select(-turfID) %>% 
  group_by(Trait_trans, moments, n) %>% 
  nest()

SC_moments_2017_clim_long <- SC_moments_2017_clim_long %>% 
  select(Trait_trans, moments, Site, turfID, Temp, Precip, value) %>% 
  group_by(Trait_trans, moments, turfID) %>% 
  mutate(n = 1:n()) %>% 
  ungroup() %>%
  select(-turfID) %>% 
  group_by(Trait_trans, moments, n) %>% 
  nest()

#### Testing with mixed effect model - TIME CONSUMING ####

results_09 <- SC_moments_2009_clim_long %>%
  mutate(model = map(data, mixed_model_temp_precip))

results_17 <- SC_moments_2017_clim_long %>%
  mutate(model = map(data, mixed_model_temp_precip))

# Tidying the model, giving the model output in a nice format. Making predicted values for each of the trait:moment combination along the climatic gradients. Calculating pseudo R squared values based on the method in paper Nakagawa et al 2017.

tidy_model_predicted_09 <- results_09 %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_without_random)) %>% 
  mutate(R_squared = map(model, rsquared))

tidy_model_predicted_17 <- results_17 %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_without_random)) %>% 
  mutate(R_squared = map(model, rsquared))

# Making a dataset with only the predicted values. Unnesting the list of the predicted values.

predicted_values_09 <- tidy_model_predicted_09 %>%
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

model_output_09 <- tidy_model_predicted_09 %>% 
  select(Trait_trans, moments, n, model_output, R_squared) %>% 
  unnest(c(model_output, R_squared)) %>% 
  filter(term %in% c("Temp", "scale(Precip)")) %>% 
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

model_output_17 <- tidy_model_predicted_17 %>% 
  select(Trait_trans, moments, n, model_output, R_squared) %>% 
  unnest(c(model_output, R_squared)) %>% 
  filter(term %in% c("Temp", "scale(Precip)")) %>% 
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