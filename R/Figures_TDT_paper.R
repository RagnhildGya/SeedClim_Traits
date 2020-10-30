#### Figures - Trait driver theory paper ####

## Libraries ##
library(ggpubr)

## Plotting trait distributions ##

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

