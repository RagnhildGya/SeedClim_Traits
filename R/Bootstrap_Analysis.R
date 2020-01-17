#### Run analysis ####

### Source ###

source("R/Cleaning.R")
source("R/Bootstraping.R")

### Libraries ###

library(broom.mixed)
library(lme4)
library(lmerTest)
library(purrr)
library(piecewiseSEM)
library(factoextra)
library(GGally)
library(ggcorrplot)
library(textshape)


set.seed(47)

## Run the bootstrapping, output community weighted trait distributions ##
# Output is the distributions pulled from the raw data weighted by cover, and it shows the distributions used to calculated mean, variance, skewness and kurtosis on. This code only gives 10 (nrep) replicates per plot, but pulls 200 leaves (samplesize) each time.

Raw_Data_Weighted <- CWM_Bootstrapping(community_cover, traitdata_1, nrep = 30, samplesize = 200, moments = FALSE)

# Adding VPD values to the dataset for ploting

Raw_Data_Weighted <- Raw_Data_Weighted %>% 
mutate(VPD = recode(Site, Ulv = 0.6371271, Alr = 0.8058131, Fau = 0.9020033, Lav = 0.5619802, Hog = 0.7866742, Vik = 0.9100686, Gud = 0.5979764, Ram = 0.6462355, Arh = 0.8156745, Skj = 0.4708173, Ves = 0.6373588, Ovs = 0.7643447))



## Run the bootstrapping and summarize each plot, and replicate with mean, variance and kurtosis ##
# Output is the summarized moments of the distributions for each bootstrapped replicate of each plot

Bootstrap_Traits <- CWM_Bootstrapping(community_cover, traitdata_1, nrep = 100)

# Adding climatic information to the dataset

Bootstrap_Traits1 <- Bootstrap_Traits %>% 
  filter(!is.na(Trait)) %>% 
  mutate(T_level = recode(Site, Ulv = 6.5, Lav = 6.5,  Gud = 6.5, Skj = 6.5, Alr = 8.5, Hog = 8.5, Ram = 8.5, Ves = 8.5, Fau = 10.5, Vik = 10.5, Arh = 10.5, Ovs = 10.5)) %>%
  mutate(Temp = recode(Site, Ulv=6.17, Lav=6.45, Gud=5.87, Skj=6.58, Alr=9.14, Hog=9.17, Ram=8.77, Ves=8.67, Fau=10.3, Vik=10.55, Arh=10.60, Ovs=10.78))%>%
  mutate(Precip= recode(Site, Ulv=596, Lav=1321, Gud=1925, Skj=2725, Alr=789, Hog=1356, Ram=1848, Ves=3029, Fau=600, Vik=1161, Arh=2044, Ovs=2923))%>%
  mutate(P_level = recode(Site, Ulv = 600, Alr = 600, Fau = 600, Lav = 1200, Hog = 1200, Vik = 1200, Gud = 2000, Ram = 2000, Arh = 2000, Skj = 2700, Ves = 2700, Ovs = 2700)) %>% 
  mutate(VPD = recode(Site, Ulv = 0.6371271, Alr = 0.8058131, Fau = 0.9020033, Lav = 0.5619802, Hog = 0.7866742, Vik = 0.9100686, Gud = 0.5979764, Ram = 0.6462355, Arh = 0.8156745, Skj = 0.4708173, Ves = 0.6373588, Ovs = 0.7643447)) %>% 
  mutate(P_cat = recode(Site, Ulv = "Dry", Alr = "Dry", Fau = "Dry", Lav = "Dry_Intermediate", Hog = "Dry_Intermediate", Vik = "Dry_Intermediate", Gud = "Wet_Intermediate", Ram = "Wet_Intermediate", Arh = "Wet_Intermediate", Skj = "Wet", Ves = "Wet", Ovs = "Wet")) %>% 
  mutate(T_cat = recode(Site, Ulv = "Alpine", Lav = "Alpine",  Gud = "Alpine", Skj = "Alpine", Alr = "Boreal", Hog = "Boreal", Ram = "Boreal", Ves = "Boreal", Fau = "Lowland", Vik = "Lowland", Arh = "Lowland", Ovs = "Lowland")) %>% 
  mutate(VPD = recode(Site, Ulv = 0.6371271, Alr = 0.8058131, Fau = 0.9020033, Lav = 0.5619802, Hog = 0.7866742, Vik = 0.9100686, Gud = 0.5979764, Ram = 0.6462355, Arh = 0.8156745, Skj = 0.4708173, Ves = 0.6373588, Ovs = 0.7643447))
#gather(key = "Moment", value = "Value", Mean, Variance, Kurtosis, Skewness)

## Calculate 95 % confidence intervals on each plot, based on the 100 replicates for each plot ##

CI_Mean_Boot_Traits <- SummarizeBootMoments(Bootstrap_Traits1)

# Adding climatic information to the dataset

CI_Mean_Boot_Traits <- CI_Mean_Boot_Traits %>% 
  filter(!is.na(Trait)) %>% 
  mutate(T_level = recode(Site, Ulv = 6.5, Lav = 6.5,  Gud = 6.5, Skj = 6.5, Alr = 8.5, Hog = 8.5, Ram = 8.5, Ves = 8.5, Fau = 10.5, Vik = 10.5, Arh = 10.5, Ovs = 10.5)) %>%
  mutate(Temp = recode(Site, Ulv=6.17, Lav=6.45, Gud=5.87, Skj=6.58, Alr=9.14, Hog=9.17, Ram=8.77, Ves=8.67, Fau=10.3, Vik=10.55, Arh=10.60, Ovs=10.78))%>%
  mutate(Precip= recode(Site, Ulv=596, Lav=1321, Gud=1925, Skj=2725, Alr=789, Hog=1356, Ram=1848, Ves=3029, Fau=600, Vik=1161, Arh=2044, Ovs=2923))%>%
  mutate(P_level = recode(Site, Ulv = 600, Alr = 600, Fau = 600, Lav = 1200, Hog = 1200, Vik = 1200, Gud = 2000, Ram = 2000, Arh = 2000, Skj = 2700, Ves = 2700, Ovs = 2700)) %>% 
  mutate(VPD = recode(Site, Ulv = 0.6371271, Alr = 0.8058131, Fau = 0.9020033, Lav = 0.5619802, Hog = 0.7866742, Vik = 0.9100686, Gud = 0.5979764, Ram = 0.6462355, Arh = 0.8156745, Skj = 0.4708173, Ves = 0.6373588, Ovs = 0.7643447)) %>% 
  mutate(P_cat = recode(Site, Ulv = "Dry", Alr = "Dry", Fau = "Dry", Lav = "Dry_Intermediate", Hog = "Dry_Intermediate", Vik = "Dry_Intermediate", Gud = "Wet_Intermediate", Ram = "Wet_Intermediate", Arh = "Wet_Intermediate", Skj = "Wet", Ves = "Wet", Ovs = "Wet")) %>% 
  mutate(T_cat = recode(Site, Ulv = "Alpine", Lav = "Alpine",  Gud = "Alpine", Skj = "Alpine", Alr = "Boreal", Hog = "Boreal", Ram = "Boreal", Ves = "Boreal", Fau = "Lowland", Vik = "Lowland", Arh = "Lowland", Ovs = "Lowland")) %>% 
  mutate(VPD = recode(Site, Ulv = 0.6371271, Alr = 0.8058131, Fau = 0.9020033, Lav = 0.5619802, Hog = 0.7866742, Vik = 0.9100686, Gud = 0.5979764, Ram = 0.6462355, Arh = 0.8156745, Skj = 0.4708173, Ves = 0.6373588, Ovs = 0.7643447))
#gather(key = "Moment", value = "Value", Mean, Variance, Kurtosis, Skewness)


#### Testing ####

### Write a code/function to run tests on all traits and all moments ###

### NEEDS DOING: Decide on which model, and write a proper function ###

mixed_model_temp_precip<-function(df) {
  lmer(Value ~ Temp * scale(Precip) + (1 | Site), data = df)
}


# mixed_model_Soil_moisture<-function(df) {
#   lmer(Value ~ Soil_moisture + (1 | Site), data = df)
# }

# mixed_model_VPD<-function(df) {
#   lmer(Value ~ VPD + (1 | Site), data = df)
# }

predict_without_random<-function(model) {
   predict(object = model, re.form=NA)
 }

# 
# mixed_model_temp<-function(df) {
#   lmer(Value ~ Temp + (1 | Site), data = df)
# }
# 
# mixed_model_precip<-function(df) {
#   lmer(Value ~ scale(Precip) + (1 | Site), data = df)
# }


## Mixed effcet model with temperature, precipitation and interaction ##

# Making  dataset ready for model 

Bootstrap_Traits2 <- Bootstrap_Traits1 %>% 
  #left_join(metaNorway %>% select(Site, VPD, bio10, bio12), by = "Site") %>% 
  #rename("Temp_summer" = "bio10", "Precip_annual" = "bio12") %>% 
  #mutate(VPD = recode(Site, Ulv = 0.6371271, Alr = 0.8058131, Fau = 0.9020033, Lav = 0.5619802, Hog = 0.7866742, Vik = 0.9100686, Gud = 0.5979764, Ram = 0.6462355, Arh = 0.8156745, Skj = 0.4708173, Ves = 0.6373588, Ovs = 0.7643447)) %>% 
  ungroup() %>% 
  gather(Moment, Value, Mean, Variance, Skewness, Kurtosis) %>% 
  select(Trait, Moment, Site, turfID, Temp, Precip, Value) %>% 
  group_by(Trait, Moment, turfID) %>% 
  mutate(n = 1:n()) %>% 
  ungroup() %>%
  select(-turfID) %>% 
  group_by(Trait, Moment, n) %>% 
  nest()

## This code was me trying to make a dataframe in which to make predicted values, needs some work, or should be dropped ##

# Predicted_data <-expand.grid(#VPD = seq(0.4, 1, length = 100),
#                              Temp = seq(5.5, 11, length = 100),
#                              Precip = seq(550, 3500, length = 100),
#                              Site = NA) %>% 
#   group_by(Trait, Moment) %>% 
#   nest() %>% 
#   rename(mynewdata = data)
# 

### This is the code to run the model on - TIME CONSUMING ###

 results <- Bootstrap_Traits2 %>%
   mutate(model = map(data, mixed_model_temp_precip))

# Tidying the model, giving the model output in a nice format. Making predicted values for each of the trait:moment combination along the climatic gradients. Calculating pseudo R squared values based on the method in paper Nakagawa et al 2017.

tidy_model_predicted <- results %>%
  mutate(model_output = map(model, tidy)) %>%
  mutate(predicted = map(model, predict_without_random)) %>% 
  mutate(R_squared = map(model, rsquared))

# Making a dataset with only the predicted values. Unnesting the list of the predicted values.

predicted_values <- tidy_model_predicted %>%
  ungroup() %>%
  select(Trait, Moment, data, predicted) %>%
  unnest(c(data, predicted)) %>%
  rename(modeled = predicted, measured = Value)

# Making a dataset with the model output and the test-statistics (R squared), and summarizing them.

model_output <- tidy_model_predicted %>% 
  select(Trait, Moment, n, model_output, R_squared) %>% 
  unnest(c(model_output, R_squared)) %>% 
  filter(term %in% c("Temp", "scale(Precip)")) %>% 
  select(Trait, Moment, term, n, estimate, Marginal, Conditional) %>% 
  ungroup() %>% 
  group_by(Trait, Moment, term) %>% 
  summarize(effect = mean(estimate),
            R2_marginal = mean(Marginal),
            R2_conditional = mean(Conditional),
            CIlow.fit = effect - sd(estimate),
            CIhigh.fit = effect + sd(estimate)) %>% 
  mutate(Significant = case_when(CIlow.fit < 0 & CIhigh.fit < 0 ~ "Negative",
                                 CIlow.fit > 0 & CIhigh.fit > 0 ~ "Positive",
                                 CIlow.fit < 0 & CIhigh.fit > 0 ~ "No"))
  



### Correlation ###

# Making data ready for correlation tests

Corr_traits <- CI_Mean_Boot_Traits %>% 
  ungroup() %>% 
  select(Site, turfID, Trait, meanMean, Precip, Temp, VPD) %>% 
  spread(key = Trait, value = meanMean) %>% 
  select(-Site) 

# Correlations 

corr <- round(cor(Corr_traits[, -1]), 1) 
head(corr[, 1:6])

# P-values 

p.mat <- cor_pmat(Corr_traits[, -1])
head(p.mat[, 1:4])

# Correlation plot
ggcorrplot(corr, hc.order = FALSE,
           type = "lower", p.mat = p.mat, lab = TRUE,)


### Ordination ###

# Making data ready for ordination


# 
# res.pca <- prcomp(PCA_boot_traits[, -(1:2)], scale = TRUE)
# 
# fviz_eig(res.pca, addlabels = TRUE) #Visualize eigenvalues/scree plot
# 
# 
# #Visualize the results for individuals (plots)
# 
# fviz_pca_ind(res.pca,
#              col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
# 
# #Visualize the results for variables (traits) with the cos2 values (contribution to the PC)
# 
# fviz_pca_var(res.pca,
#              col.var = "contrib", # Color by contributions to the PC
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
# 
# #ggsave("PCA.jpg", width = 25 , height = 15, units = "cm")
# 
# #Make a biplot of individuals and variables
# 
# fviz_pca_biplot(res.pca, repel = TRUE,
#                 col.var = "#2E9FDF", # Variables color
#                 col.ind = "#696969",  # Individuals color
#                 label = "var"
# )
# 
# #ggsave("PCA_communitypoints.jpg", width = 15 , height = 15, units = "cm")
# 
# #Visualize individuals, add ellpises with tempereature and preacipitation
# 
# fviz_pca_ind(res.pca,
#              label = "none",
#              habillage = PCA_boot_traits$T_cat,
#              addEllipses=TRUE, ellipse.level=0.95
# )
# #ggsave("PCA_temp.jpg", width = 21 , height = 15, units = "cm")
# 
# fviz_pca_ind(res.pca,
#              label = "none",
#              habillage = PCA_boot_traits$P_cat,
#              addEllipses=TRUE, ellipse.level=0.95
# )
# #ggsave("PCA_precip.jpg", width = 21 , height = 15, units = "cm")
# 
# fviz_pca_biplot(res.pca, 
#                 col.ind = PCA_boot_traits$P_cat, 
#                 addEllipses = TRUE, label = "var",
#                 col.var = "black", repel = TRUE,
#                 legend.title = "Temperature") 
# 
# #ggsave("PCA_precip_with_traitaxes.jpg", width = 21 , height = 15, units = "cm")
# 
# fviz_pca_biplot(res.pca, 
#                 col.ind = PCA_boot_traits$T_cat, 
#                 addEllipses = TRUE, label = "var",
#                 col.var = "black", repel = TRUE,
#                 legend.title = "Temperature") 
# 
# #ggsave("PCA_temp_with_traitaxes.jpg", width = 21 , height = 15, units = "cm")
# 
# # Eigenvalues
# eig.val <- get_eigenvalue(res.pca) #Extract the eigenvalues/variances of principal components
# eig.val
# 
# 
# res.var <- get_pca_var(res.pca) #Extract the results for variables
# 
# res.var$coord          # Coordinates
# res.var$contrib        # Contributions to the PCs
# res.var$cos2           # Quality of representation
# 
# 
# res.ind <- get_pca_ind(res.pca) #Extract the results for individuals
# 
# res.ind$coord          # Coordinates
# res.ind$contrib        # Contributions to the PCs
# res.ind$cos2           # Quality of representation 
# 
# ggcorrplot(res.var$cos2, method = "circle")
# #ggsave("PCA_dimentions_traits.jpg", width = 18 , height = 15, units = "cm")
# 
# fviz_cos2(res.pca, choice = "var", axes = 1)
# fviz_cos2(res.pca, choice = "var", axes = 2)
# 
# 
# library("corrplot")
# corrplot(res.var$cos2, is.corr=FALSE)
# corrplot(res.var$contrib, is.corr=FALSE)  #Contribution to the PCs
# 
# # Contributions of variables to PC1
# fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# # Contributions of variables to PC2
# fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# 
# library("FactoMineR")
# res.desc <- dimdesc(res.pca, proba = 0.05)
# # Description of dimension 1
# res.desc$Dim.1


library(vegan)
library("ggvegan")


Ord_boot_traits <- CI_Mean_Boot_Traits %>% 
  ungroup() %>% 
  filter(!Trait == "Wet_Mass_g_log") %>% 
  select(turfID, Trait, Temp, Precip, VPD, meanMean) %>% 
  #gather(Moment, Value, -(turfID:P_cat)) %>% 
  #unite(temp, Trait, Moment) %>% 
  spread(key = Trait, value = meanMean) %>% 
  column_to_rownames("turfID")

RDA <- rda(Ord_boot_traits[, -(1:3)]~ Temp+Precip, scale = TRUE, data = Ord_boot_traits)

autoplot(RDA) +
  theme_bw()

autoplot(RDA, arrows = TRUE, data = PCA_boot_traits) +
  scale_x_continuous(expand = c(0.22, 0)) +
  geom_point(data = RDA, aes(RDA1, RDA2), size=2, alpha=0.5) +
  geom_abline(intercept = 0,slope = 0,linetype="dashed", size=0.8) +
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8) + labs(x = "Axis 1", y="Axis 2") + 
  theme_bw()

ggplot(RDA_fort, aes(x = RDA1, y = RDA2)) +
  geom_point(show.legend = FALSE) +
  scale_size(range = 2) +
  coord_equal()

RDA_fort <- fortify(RDA)

RDA
plot(RDA)
screeplot(RDA)

coef(RDA)

RsquareAdj(RDA)$adj.r.squared

# Code that doesn't work for contrained analysis
# biplot(RDA, scaling = "symmetric")
# RsquareAdj(RDA)

# rda_fort <- fortify(RDA)
# autoplot(RDA)


#### Old code ####

# There were two codes for making the dataset ready for the model, I think this was the old one and that could maybe be deleted, keeping it here in case I need it later. 

# Bootstrap_Traits2 <- Bootstrap_Traits1 %>% 
#   ungroup() %>%
#   #left_join(metaNorway %>% select(Site, VPD, bio10, bio12), by = "Site") %>% 
#   #rename("Temp_summer" = "bio10", "Precip_annual" = "bio12") %>% 
#   gather(Moment, Value, Mean, Variance, Skewness, Kurtosis) %>% 
#   # pivot_longer(cols %in% ("Mean", "Variance", "Skewness", "Kurtosis"),
#   #              names_to = "Moments",
#   #              values_to = "Value")
#   select(Trait, Moment, Site, VPD, Value) %>% 
#   group_by(Trait, Moment) %>% 
#   nest() 
