#### Figures - Trait driver theory paper ####

## Libraries ##
library(ggpubr)
library(ggridges)
library(ggfortify)
library(corrplot)

## Climate figure ##

ggplot(aes(x = Precip_decade, y = Temp_decade,
           color = Precip_level, fill = Precip_level, shape = Temp_level), data = env) +
  geom_segment(aes(x = Precip_decade, y = Temp_decade, yend = Temp_century, xend = Precip_century)) +
  geom_point(aes(x = Precip_century, y = Temp_century, size = 4, shape = "1960-1990 mean")) +
  geom_pointrange(aes(ymin = Temp_decade-Temp_se, ymax = Temp_decade+Temp_se)) +
  geom_errorbarh(aes(xmin = Precip_decade-Precip_se, xmax = Precip_decade+Precip_se)) +
  labs(x = "Annual precipitation in mm", y = "Tetraterm temperature in °C") +
  scale_color_manual(name = "Precipitation level", values = c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")) +
  scale_fill_manual(name = "Precipitation level", values = c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")) +
  #scale_color_brewer(name = "Precipitation level", palette = "Blues") + 
  scale_shape_manual(name = "Temperature level", values = c(25, 1, 21, 24)) +
  guides(fill = "none", size = "none") +
  theme_minimal(base_size = 20)

#ggsave("SeedClim_climate_over_time.jpg", width = 22 , height = 14, units = "cm")


ggplot(aes(x = Precip_deviation_decade, y = Temp_deviation_decade, color = as.factor(Year)), data = env) +
  geom_point() +
  geom_vline(xintercept =  0) +
  geom_hline(yintercept =  0) +
  labs(x = "Deviation from the decade mean annual precipitation (mm)", y = "Deviation from the devade mean tetraterm temperature (°C)", color = "Year") +
  theme_minimal(base_size = 15)

#ggsave("SeedClim_climate_deviation.jpg", width = 18 , height = 18, units = "cm")

## Plotting trait distributions ##
set.seed(47)

T_names <- c(
  "6.5" = "Alpine", 
  "8.5" = "Sub-alpine",
  "10.5" = "Boreal")

slice_sample(SeedClim_traits_allYears, n = 200,  
             replace = TRUE, weight_by = weight) %>% 
  left_join(env, by = c("Site" = "Site")) %>% 
  filter(Trait_trans == "SLA_cm2_g") %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(x = Value, y = fct_rev(year), fill = as.factor(Temp_level))) +
  geom_density_ridges(bandwidth = 20) +
  facet_wrap(~Temp_level, nrow = 1, labeller = labeller(Temp_level = T_names)) +
  theme_minimal(base_size = 15) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  guides(fill=FALSE) +
  labs(x = "Specific leaf area (cm2/g)", y = "Year")

# ggsave("SLA_distributions_over_time.jpg", width = 30 , height = 20, units = "cm")

## Probability distribution of community weighted mean SLA  in 2009 ##


trait_distribution_plot <- function(dat, year, trait){
  
  plot <- dat %>% 
    ungroup() %>% 
    filter(Trait_trans == trait) %>% 
    ggplot(aes(mean, fill = as.factor(T_level)))+
    geom_density(alpha = 0.5)+
    labs(x = trait, fill = "Summer temp", title = year)+
    theme_bw(base_size = 12)+
    scale_fill_brewer(palette = "YlOrRd") +
    xlim(110,350) +
    guides(fill=FALSE)
  
  return(plot)
}

plot1 <- trait_distribution_plot(dat = summarised_boot_moments_climate_2009, trait = "SLA_cm2_g", year = "2009")
plot2 <- trait_distribution_plot(dat = summarised_boot_moments_climate_2011, trait = "SLA_cm2_g", year = "2011")
plot3 <- trait_distribution_plot(dat = summarised_boot_moments_climate_2012, trait = "SLA_cm2_g", year = "2012")
plot4 <- trait_distribution_plot(dat = summarised_boot_moments_climate_2013, trait = "SLA_cm2_g", year = "2013")
plot5 <- trait_distribution_plot(dat = summarised_boot_moments_climate_2015, trait = "SLA_cm2_g", year = "2015")
plot6 <- trait_distribution_plot(dat = summarised_boot_moments_climate_2017, trait = "SLA_cm2_g", year = "2017")

SLA_distributions <- ggarrange(plot1, plot2, plot3, plot4, plot5, plot6,
                      ncol = 3, nrow = 2)

annotate_figure(SLA_distributions, top = text_grob("SLA over time", face = "bold", size = 14))

ggsave("SLA_distributions_over_time.jpg", width = 30 , height = 20, units = "cm")

SC_moments_clim_allYears %>% 
  filter(Trait_trans == "SLA_cm2_g") %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(x = value, y = fct_rev(year), group = year))+
  geom_density_ridges() +
  facet_grid(~moments, scales = "free") +
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(x = "SLA (cm2/g)", title = "Probability distribution of moments of SLA distribuion", y = "Year")+
  theme_bw(base_size = 12)

ggsave("SLA_moments_over_time.jpg", width = 30 , height = 20, units = "cm")

## Variance, skewness, kurtosis ##

summarised_boot_moments_climate_2017 %>% 
  filter(Trait_trans == "SLA_cm2_g") %>% 
  ggplot(aes(var, fill = as.factor(T_level)))+
  geom_density(alpha = 0.5)+
  #facet_wrap(~P_cat, nrow = 1) +
  labs(x = "Community weighted variance - SLA (cm2/g)", fill = "Temperature category")+
  theme_bw(base_size = 12)+
  scale_fill_brewer(palette = "YlOrRd")

summarised_boot_moments_climate_2009 %>% 
  filter(Trait_trans == "SLA_cm2_g") %>% 
  ggplot(aes(skew, fill = as.factor(T_level)))+
  geom_density(alpha = 0.5)+
  #facet_wrap(~P_cat, nrow = 1) +
  labs(x = "Community weighted skewness - SLA (cm2/g)", fill = "Temperature category")+
  theme_bw(base_size = 12)+
  scale_fill_brewer(palette = "YlOrRd")

summarised_boot_moments_climate_2017 %>% 
  filter(Trait_trans == "SLA_cm2_g") %>% 
  ggplot(aes(kurt, fill = as.factor(T_level)))+
  geom_density(alpha = 0.5)+
  #facet_wrap(~P_cat, nrow = 1) +
  labs(x = "Community weighted kurtosis - SLA (cm2/g)", fill = "Temperature category")+
  geom_vline(xintercept = 1.2) +
  theme_bw(base_size = 12) +
  scale_fill_brewer(palette = "YlOrRd")

#### Ordination ####


#Visualize the results for variables (traits) with the cos2 values (contribution to the PC)
corrplot(var$cos2, is.corr = FALSE)

# var_leaf <- get_pca_var(pca_leaf_economic)
# corrplot(var_leaf$cos2, is.corr = FALSE)
# 
# var_size <- get_pca_var(pca_size)
# corrplot(var_size$cos2, is.corr = FALSE)

fviz_cos2(pca_trait, choice = "var")
fviz_contrib(pca_trait, choice = "var", axes = 1)
fviz_contrib(pca_trait, choice = "var", axes = 2)


fviz_eig(pca_trait, addlabels = TRUE) #Visualize eigenvalues/scree plot
fviz_eig(pca_leaf_economic, addlabels = TRUE)
fviz_eig(pca_size, addlabels = TRUE)

# fviz_pca_var(pca_trait,
#              col.var = "contrib", # Color by contributions to the PC
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )

fviz_pca_biplot(pca_trait, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969",  # Individuals color
                label = "var",
                labelsize = 5, 
                habillage = Ord_boot_traits$Temp_level,
                ellipse.level = 0.95) +
  theme_minimal(base_size = 20) +
  guides(shape = FALSE) +
  scale_color_manual(name = "Summer temperature", values = c("#8DD5E1", "#FCB971", "#B93B3B")) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14))+
  xlim(-5, 5) + 
  ylim (-5, 5)

#ggsave("PCA_all_years_with_temp.jpg", width = 22 , height = 16, units = "cm")


fviz_pca_biplot(pca_leaf_economic, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969",  # Individuals color
                label = "var",
                labelsize = 5,
                habillage = Ord_boot_traits$Temp_level,
                addEllipses = TRUE,
                ellipse.level = 0.95) +
  theme_minimal(base_size = 20)

fviz_pca_biplot(pca_size, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969",  # Individuals color
                label = "var",
                labelsize = 5,
                habillage = Ord_boot_traits$Temp_level,
                addEllipses = TRUE,
                ellipse.level = 0.95) +
  theme_minimal(base_size = 20)
