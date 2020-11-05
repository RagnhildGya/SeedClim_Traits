#### Figures - Trait driver theory paper ####

## Libraries ##
library(ggpubr)
library(ggridges)

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
  filter(!year == "2010") %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(x = Value, y = fct_rev(year), fill = as.factor(T_level))) +
  stat_density_ridges(quantile_lines = TRUE) +
  facet_wrap(~T_level, scales = "free", nrow = 1, labeller = labeller(T_level = T_names)) +
  theme_minimal(base_size = 15) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  guides(fill=FALSE) +
  labs(x = "Specific leaf area (cm2/g) distribution", y = "Year")

ggsave("SLA_distributions_over_time_with_quantilles.jpg", width = 30 , height = 20, units = "cm")

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
  ggplot(aes(x = value, y = year, group = year))+
  geom_density_ridges() +
  facet_grid(~moments, scales = "free") +
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(x = "SLA (cm2/g)", title = "Probability distribution of moments of SLA distribuion")+
  theme_bw(base_size = 12)

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

fviz_eig(res.pca_09, addlabels = TRUE) #Visualize eigenvalues/scree plot

fviz_pca_var(res.pca_09,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca_09, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969",  # Individuals color
                label = "var",
                labelsize = 5) +
  theme_minimal(base_size = 20)

