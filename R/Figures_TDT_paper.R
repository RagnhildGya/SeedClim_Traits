#### Figures - Trait driver theory paper ####

## Libraries ##
library(ggpubr)
library(ggridges)
library(ggfortify)
library(corrplot)
library(svglite)
library(grid)
library(gridExtra)

## Color palettes ##
Temp_palette <- c("#d8c593", "#dd7631", "#bb3b0e")
Precip_palette <- c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")

## Without intraspecific plot ##

plot1 <- sum_moments_climate_without_intra %>% 
  filter(Trait_trans == "SLA_cm2_g_log") %>% 
ggplot(aes(x = Precip_yearly, y = mean, color = Temp_level))+
  geom_point() +
  geom_smooth(method = "lm")

plot2 <- sum_moments_climate_fullcommunity %>% 
  filter(Trait_trans == "SLA_cm2_g_log") %>% 
  ggplot(aes(x = Precip_yearly, y = mean, color = Temp_level))+
  geom_point() +
  geom_smooth(method = "lm")

plot1 + plot2


## Climate figure ##

env %>% 
  mutate(Temp_old = recode(Temp_level, "10.5" = "Boreal 1960-90",
                           "8.5" = "Sub-alpine 1960-90",
                           "6.5" = "Alpine 1960-90")) %>% 
  mutate(Temp_level = recode(Temp_level, "10.5" = "Boreal 2009-19",
                           "8.5" = "Sub-alpine 2009-19",
                           "6.5" = "Alpine 2009-19")) %>% 
ggplot(aes(x = Precip_decade, y = Temp_decade,
           color = Precip_level, fill = Precip_level, shape = Temp_level)) +
  geom_point(aes(x = Precip_century, y = Temp_century, shape = Temp_old, size = 3)) +
  geom_segment(aes(x = Precip_decade, y = Temp_decade, yend = Temp_century, xend = Precip_century)) +
  geom_pointrange(aes(ymin = Temp_decade-Temp_se, ymax = Temp_decade+Temp_se)) +
  geom_errorbarh(aes(xmin = Precip_decade-Precip_se, xmax = Precip_decade+Precip_se)) +
  labs(x = "Annual precipitation in mm", y = "Tetraterm temperature in °C") +
  scale_color_manual(name = "Precipitation", values = c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")) +
  scale_fill_manual(name = "Precipitation", values = c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")) +
  scale_shape_manual(name = "Temperature", values = c(2, 24, 6, 25, 1, 21)) + 
  guides(fill = "none", size = "none") +
  theme_minimal(base_size = 20)


#ggsave("SeedClim_climate_over_time.jpg", width = 22 , height = 14, units = "cm")

env %>% 
  mutate(Year = as.factor(Year)) %>% 
  #filter(Year %in% c("2009", "2011", "2012", "2013", "2015", "2017")) %>% 
ggplot(aes(x = Precip_deviation_decade, y = Temp_deviation_decade, color = as.factor(Year))) +
  geom_point() +
  geom_vline(xintercept =  0) +
  geom_hline(yintercept =  0) +
  labs(x = "Deviation from the decade mean annual precipitation (mm)", y = "Deviation from the devade mean tetraterm temperature (°C)", color = "Year") +
  theme_minimal(base_size = 15)

env %>% 
  mutate(Year = as.factor(Year)) %>% 
  #filter(Year %in% c("2009", "2011", "2012", "2013", "2015", "2017")) %>% 
ggplot(aes(x = Year, y = Temp_yearly_prev)) +
  geom_point() +
  facet_wrap(~Temp_level)

ggplot(aes(x = Year, y = Precip_yearly), data = env) +
  geom_point() +
  facet_wrap(~Precip_level)

#ggsave("SeedClim_climate_deviation.jpg", width = 18 , height = 18, units = "cm")

## Plotting trait distributions ##
set.seed(47)

T_names <- c(
  "6.5" = "Alpine", 
  "8.5" = "Sub-alpine",
  "10.5" = "Boreal")

trait_names <- c(
  "CN_ratio_log" = "C/N ratio",
  "Leaf_Area_cm2_log" = "Leaf Area (cm2)",
  "Plant_Height_mm_log" = "Plant height (mm)",
  "SLA_cm2_g_log" = "SLA (cm2/g)")

bandwidthsize <- c(0.1, 0.2, 0.2, 0.1)

a <- slice_sample(SeedClim_traits_allYears, n = 200,  
             replace = TRUE, weight_by = weight) %>% 
  left_join(env, by = c("Site" = "Site")) %>% 
  filter(Trait_trans %in% c("SLA_cm2_g_log", "LDMC", "Leaf_Area_cm2_log", "Plant_Height_mm_log")) %>% 
  mutate(year = as.factor(year))

SLA_density <- a %>% 
  filter(Trait_trans == "SLA_cm2_g_log") %>% 
  ggplot(aes(x = Value, y = fct_rev(year), fill = fct_rev(as.factor(Temp_level)))) +
  geom_density_ridges2(bandwidth = 0.08, alpha = 0.5, scale = 0.95) +
  theme_minimal(base_size = 12) +
  scale_fill_manual(name = "Temperature level", values = c("#fe1100", "#fe875d", "#356288"), labels = c("Boreal", "Sub-alpine", "Alpine")) + 
  labs(y = "Year", title = "SLA (cm2/g)") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5)) 
  
LDMC_density <- a %>% 
  filter(Trait_trans == "LDMC") %>% 
  ggplot(aes(x = Value, y = fct_rev(year), fill = fct_rev(as.factor(Temp_level)))) +
  geom_density_ridges2(bandwidth = 0.02, alpha = 0.5, scale = 0.95) +
  scale_fill_manual(name = "Temperature level", values = c("#fe1100", "#fe875d", "#356288"), labels = c("Boreal", "Sub-alpine", "Alpine")) + 
  xlim(0, 0.8) +
  labs(title = "LDMC") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5)) 
  
Leaf_area_density <- a %>% 
  filter(Trait_trans == "Leaf_Area_cm2_log") %>% 
  ggplot(aes(x = Value, y = fct_rev(year), fill = fct_rev(as.factor(Temp_level)))) +
    geom_density_ridges2(bandwidth = 0.15, alpha = 0.5, scale = 0.95) +
    theme_minimal(base_size = 12) +
    scale_fill_manual(name = "Temperature level", values = c("#fe1100", "#fe875d", "#356288"), labels = c("Boreal", "Sub-alpine", "Alpine")) + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Leaf Area (cm2)")

Plant_height_density <- a %>% 
  filter(Trait_trans == "Plant_Height_mm_log") %>% 
ggplot(aes(x = Value, y = fct_rev(year), fill = fct_rev(as.factor(Temp_level)))) +
    geom_density_ridges2(bandwidth = 0.15, alpha = 0.5, scale = 0.95) +
    theme_minimal(base_size = 12) +
    scale_fill_manual(name = "Temperature level", values = c("#fe1100", "#fe875d", "#356288"), labels = c("Boreal", "Sub-alpine", "Alpine")) + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Plant height (mm)")

ggarrange(SLA_density,
          LDMC_density + rremove("y.text"), 
          Leaf_area_density + rremove("y.text"), 
          Plant_height_density + rremove("y.text"),
          ncol = 4, nrow = 1, common.legend = TRUE, legend = "bottom")

# ggsave("Trait_distributions_over_time.jpg", width = 35 , height = 20, units = "cm")

### Community weighted moments probability distributions ###

moments_clim_long_fullcommunity %>% 
  filter(Trait_trans == "SLA_cm2_g_log") %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(x = value, y = fct_rev(year), group = year)) +
  geom_density_ridges() +
  facet_grid(~moments, scales = "free") +
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(x = "SLA (cm2/g)", title = "Probability distribution of moments of SLA distribuion", y = "Year") +
  theme_bw(base_size = 12)

ggsave("SLA_moments_over_time.jpg", width = 30 , height = 20, units = "cm")


#### Ordination ####


#Visualize the results for variables (traits) with the cos2 values (contribution to the PC)
corrplot(var$cos2, is.corr = FALSE)

contr_traits <- as.data.frame(var$cos2) %>% 
  select(Dim.1, Dim.2) %>% 
  rownames_to_column("trait") %>% 
  group_by(trait) %>% 
  mutate(contribution = sum(Dim.1, Dim.2))

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

#### Ordination with traits ####

fviz_pca_biplot(pca_trait, repel = TRUE,
                col.var = "#2A2A2A", # Variables color
                col.ind = "#AAB7B8",
                label = "var",
                labelsize = 5) +
  theme_minimal(base_size = 15) +
  guides(shape = FALSE) +
  #scale_color_manual(name = "Summer temperature", values = c("#8DD5E1", "#FCB971", "#B93B3B")) +
  theme(plot.title = element_blank()) +
  coord_fixed()

ggsave("Ordination_LES_Size.svg", width = 22 , height = 16, units = "cm", dpi = 600)

#### Ordination with temperature ####

fviz_pca_ind(pca_trait, repel = TRUE,
                col.var = "#2A2A2A", # Variables color
                label = "none",
                labelsize = 5, 
                habillage = Ord_boot_traits$Temp_level, 
                addEllipses = TRUE,
                ellipse.level = 0.95,
                palette = c("#d8c593", "#dd7631", "#bb3b0e")) +
  theme_minimal(base_size = 15) +
  #scale_color_manual(name = "Summer temperature", values = c("#8DD5E1", "#FCB971", "#B93B3B")) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_blank(), axis.title = element_blank()) +
  labs(fill = "Summer temperature", color = "Summer temperature", shape = "Summer temperature") +
  coord_fixed()

ggsave("Ordination_Temp.svg", width = 18 , height = 11, units = "cm", dpi = 600)


### Ordination with precipitation ####

fviz_pca_ind(pca_trait, repel = TRUE,
              col.var = "#2A2A2A", # Variables color
              label = "none",
              labelsize = 5, 
              habillage = Ord_boot_traits$Precip_level, 
              addEllipses = TRUE,
              ellipse.level = 0.95,
              palette = c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")) +
  theme_minimal(base_size = 15) +
    #scale_color_manual(name = "Summer temperature", values = c("#8DD5E1", "#FCB971", "#B93B3B")) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_blank(), axis.title = element_blank()) +
  labs(fill = "Yearly precipitation", color = "Yearly precipitation", shape = "Yearly precipitation") +
  coord_fixed()

ggsave("Ordination_Precip.svg", width = 18 , height = 11, units = "cm", dpi = 600)
  
## Ordination over time ##
  
  
pca_fort <- fortify(pca_trait, display = "sites") %>% 
  bind_cols(Ord_boot_traits[1:6])

pca_fort %>% 
ggplot(aes(x = PC1, y = PC2, colour = Temp_level, group = turfID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2009, 1, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_color_manual(name = "Summer temperature", values = c("#8DD5E1", "#FCB971", "#B93B3B")) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 12)

#ggsave("PCA_all_years_with_temp.jpg", width = 22 , height = 16, units = "cm")


# fviz_pca_biplot(pca_leaf_economic, repel = TRUE,
#                 col.var = "#2E9FDF", # Variables color
#                 col.ind = "#696969",  # Individuals color
#                 label = "var",
#                 labelsize = 5,
#                 habillage = Ord_boot_traits$Temp_level,
#                 addEllipses = TRUE,
#                 ellipse.level = 0.95) +
#   theme_minimal(base_size = 20)
# 
# fviz_pca_biplot(pca_size, repel = TRUE,
#                 col.var = "#2E9FDF", # Variables color
#                 col.ind = "#696969",  # Individuals color
#                 label = "var",
#                 labelsize = 5,
#                 habillage = Ord_boot_traits$Temp_level,
#                 addEllipses = TRUE,
#                 ellipse.level = 0.95) +
#   theme_minimal(base_size = 20)

time <- model_output_time %>% 
  filter(moments == "mean") %>% 
  select(Trait_trans, moments, term, effect, std.error, p.value) %>% 
  mutate(moments = "time")

without_intra <- model_output_space_without_intra %>% 
  filter(moments == "mean") %>% 
  select(Trait_trans, moments, term, effect, std.error, p.value) %>% 
  mutate(moments = "without_intra")

time_skewness <- model_output_time %>% 
  filter(moments == "skewness") %>% 
  select(Trait_trans, moments, term, effect, std.error, p.value) %>% 
  mutate(moments = "time")

time_var_kurt <- model_output_time %>% 
  filter(moments %in% c("variance", "kurtosis")) %>% 
  select(Trait_trans, moments, term, effect, std.error, p.value) %>% 
  mutate(moments = recode(moments, "variance" = "variance_time",
                        "kurtosis" = "kurtosis_time"))

model_output_space %>% 
  bind_rows(time) %>% 
  mutate(Trait_moment = paste0(moments, "_", Trait_trans)) %>% 
  mutate(trend_col = ifelse(effect > 0, "positive", "negative")) %>% 
  mutate(significance = ifelse(p.value > 0.05, "Non significant", "Significant")) %>% 
  filter(moments %in% c("mean", "kurtosis", "time")) %>% 
  mutate(moments = factor(moments, levels = c("mean", "time", "kurtosis"))) %>% 
  mutate(term = as.factor(recode(term, "Temp_yearly_prev" = "PreviousYearTemp", "scale(Precip_yearly)" = "Precipitation", "Temp_yearly_spring" = "SpringTemp", "Temp_yearly_prev:scale(Precip_yearly)" = "PreviousYearTemp:Precipitation", "scale(Precip_yearly):Temp_yearly_spring" ="Precipitation:SpringTemp"))) %>% 
  mutate(term = factor(term, levels = c("PreviousYearTemp", "Precipitation", "SpringTemp", "PreviousYearTemp:Precipitation", "Precipitation:SpringTemp"))) %>% 
  ggplot(aes(x = effect, y = Trait_trans, color = trend_col, shape = significance)) +
  geom_point(size = 4)+
  geom_pointrange(aes(xmin = effect-std.error, xmax = effect+std.error)) +
  facet_grid(term ~ moments, scales = "free") +
  scale_color_manual(values = c("#8DD5E1", "#B93B3B")) +
  scale_shape_manual(values = c(1,19)) +
  geom_vline(xintercept =  0) +
  theme_bw()

## Mean ##

model_output_space %>% 
  bind_rows(time) %>%
  bind_rows(without_intra) %>% 
  mutate(Trait_moment = paste0(moments, "_", Trait_trans)) %>% 
  mutate(trend_col = ifelse(effect > 0, "positive", "negative")) %>% 
  mutate(significance = ifelse(p.value > 0.05, "Non significant", "Significant")) %>% 
  filter(moments %in% c("mean", "time", "without_intra")) %>% 
  mutate(moments = factor(moments, levels = c("time", "without_intra", "mean"))) %>% 
  mutate(coloring = paste0(moments, "", significance)) %>% 
  mutate(coloring = factor(coloring, levels = c("timeNon significant", "timeSignificant", "without_intraNon significant", "without_intraSignificant", "meanNon significant", "meanSignificant" ))) %>% 
  mutate(term = as.factor(recode(term, "Temp_yearly_prev" = "PreviousYearTemp", "scale(Precip_yearly)" = "Precipitation", "Temp_yearly_spring" = "SpringTemp", "Temp_yearly_prev:scale(Precip_yearly)" = "PreviousYearTemp:Precipitation", "scale(Precip_yearly):Temp_yearly_spring" ="Precipitation:SpringTemp"))) %>% 
  mutate(term = factor(term, levels = c("PreviousYearTemp", "Precipitation", "SpringTemp", "PreviousYearTemp:Precipitation", "Precipitation:SpringTemp"))) %>% 
  mutate(Trait_trans = factor(Trait_trans, levels = c("Leaf_Area_cm2_log", "Plant_Height_mm_log", "Dry_Mass_g_log", "Wet_Mass_g_log", "C_percent", "SLA_cm2_g_log", "N_percent", "CN_ratio_log", "LDMC", "Leaf_Thickness_Ave_mm"))) %>%
  ggplot(aes(x = fct_rev(Trait_trans), y = effect, fill = coloring, color = moments)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6), width = 0.7) +
  #geom_bar_pattern(aes(pattern = 'stripe'), stat = "identity", position = "dodge") +
  #geom_errorbar(aes(xmin = effect-std.error, xmax = effect+std.error)) +
  facet_grid(~term, scales = "free") +
  scale_color_manual(values = c("#89B7E1", "#5693CA", "#2E75B6")) +
  scale_fill_manual(values = c("#EDEDED", "#89B7E1", "#EDEDED", "#5693CA", "#EDEDED", "#2E75B6")) +
  #scale_shape_manual(values = c(1,19)) +
  geom_hline(yintercept =  0) +
  theme_bw() +
  coord_flip() +
  guides(fill = "none", color = "none", size = "none") +
  theme(axis.title.y=element_blank())

## Skewness ## Appendix figure?

model_output_space %>% 
  bind_rows(time_skewness) %>% 
  mutate(Trait_moment = paste0(moments, "_", Trait_trans)) %>% 
  mutate(trend_col = ifelse(effect > 0, "positive", "negative")) %>% 
  mutate(significance = ifelse(p.value > 0.05, "Non significant", "Significant")) %>% 
  filter(moments %in% c("skewness", "time")) %>% 
  mutate(moments = factor(moments, levels = c("time", "skewness"))) %>% 
  mutate(coloring = paste0(moments, "", significance)) %>% 
  mutate(coloring = factor(coloring, levels = c("timeNon significant", "timeSignificant", "skewnessNon significant", "skewnessSignificant"))) %>% 
  mutate(term = as.factor(recode(term, "Temp_yearly_prev" = "PreviousYearTemp", "scale(Precip_yearly)" = "Precipitation", "Temp_yearly_spring" = "SpringTemp", "Temp_yearly_prev:scale(Precip_yearly)" = "PreviousYearTemp:Precipitation", "scale(Precip_yearly):Temp_yearly_spring" ="Precipitation:SpringTemp"))) %>% 
  mutate(term = factor(term, levels = c("PreviousYearTemp", "Precipitation", "SpringTemp", "PreviousYearTemp:Precipitation", "Precipitation:SpringTemp"))) %>% 
  mutate(Trait_trans = factor(Trait_trans, levels = c("Leaf_Area_cm2_log", "Plant_Height_mm_log", "Dry_Mass_g_log", "Wet_Mass_g_log", "C_percent", "SLA_cm2_g_log", "N_percent", "CN_ratio_log", "LDMC", "Leaf_Thickness_Ave_mm"))) %>%
  ggplot(aes(x = fct_rev(Trait_trans), y = effect, fill = coloring, color = moments)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6), width = 0.7) +
  #geom_bar_pattern(aes(pattern = 'stripe'), stat = "identity", position = "dodge") +
  #geom_errorbar(aes(xmin = effect-std.error, xmax = effect+std.error)) +
  facet_grid(~term, scales = "free") +
  scale_color_manual(values = c("#89B7E1", "#2E75B6")) +
  scale_fill_manual(values = c("#EDEDED", "#89B7E1", "#EDEDED", "#2E75B6")) +
  #scale_shape_manual(values = c(1,19)) +
  geom_hline(yintercept =  0) +
  theme_bw() +
  coord_flip() +
  guides(fill = "none", color = "none", size = "none") +
  theme(axis.title.y=element_blank())

## Variance & kurtosis figure ##

model_output_space %>% 
  bind_rows(time_var_kurt) %>% 
  mutate(Trait_moment = paste0(moments, "_", Trait_trans)) %>% 
  mutate(trend_col = ifelse(effect > 0, "positive", "negative")) %>% 
  mutate(significance = ifelse(p.value > 0.05, "Non significant", "Significant")) %>% 
  filter(moments %in% c("kurtosis", "variance", "kurtosis_time", "variance_time")) %>% 
  mutate(moments = factor(moments, levels = c("kurtosis", "kurtosis_time", "variance", "variance_time"))) %>% 
  mutate(coloring = paste0(moments, "", significance)) %>% 
  mutate(coloring = factor(coloring, levels = c("kurtosisNon significant", "kurtosisSignificant", "kurtosis_timeNon significant", "kurtosis_timeSignificant", "varianceNon significant", "varianceSignificant", "variance_timeNon significant", "variance_timeSignificant"))) %>% 
  mutate(term = as.factor(recode(term, "Temp_yearly_prev" = "PreviousYearTemp", "scale(Precip_yearly)" = "Precipitation", "Temp_yearly_spring" = "SpringTemp", "Temp_yearly_prev:scale(Precip_yearly)" = "PreviousYearTemp:Precipitation", "scale(Precip_yearly):Temp_yearly_spring" ="Precipitation:SpringTemp"))) %>% 
  mutate(term = factor(term, levels = c("PreviousYearTemp", "Precipitation", "SpringTemp", "PreviousYearTemp:Precipitation", "Precipitation:SpringTemp"))) %>% 
  mutate(Trait_trans = factor(Trait_trans, levels = c("Leaf_Area_cm2_log", "Plant_Height_mm_log", "Dry_Mass_g_log", "Wet_Mass_g_log", "C_percent", "SLA_cm2_g_log", "N_percent", "CN_ratio_log", "LDMC", "Leaf_Thickness_Ave_mm"))) %>%
  ggplot(aes(x = fct_rev(Trait_trans), y = effect, fill = moments, color = moments)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6), width = 0.7) +
  #geom_bar_pattern(aes(pattern = 'stripe'), stat = "identity", position = "dodge") +
  #geom_errorbar(aes(xmin = effect-std.error, xmax = effect+std.error)) +
  facet_grid(~term, scales = "free") +
  scale_color_manual(values = c("#89B7E1", "#2E75B6", "#A3D08F", "#67855A")) +
  scale_fill_manual(values = c("#89B7E1", "#2E75B6", "#A3D08F","#67855A")) +
  #scale_shape_manual(values = c(1,19)) +
  geom_hline(yintercept =  0) +
  theme_bw() +
  coord_flip() +
  guides(size = "none") +
  theme(axis.title.y=element_blank())


mean_time_space_skewness_plot <- model_output_space %>% 
  bind_rows(time) %>% 
  mutate(Trait_moment = paste0(moments, "_", Trait_trans)) %>% 
  mutate(trend_col = ifelse(effect > 0, "positive", "negative")) %>% 
  mutate(significance = ifelse(p.value > 0.05, "Non significant", "Significant")) %>% 
  filter(moments %in% c("mean", "skewness", "time")) %>% 
  mutate(moments = factor(moments, levels = c("skewness", "time", "mean"))) %>% 
  mutate(coloring = paste0(moments, "", significance)) %>% 
  mutate(coloring = factor(coloring, levels = c("skewnessNon significant", "skewnessSignificant", "timeNon significant", "timeSignificant", "meanNon significant", "meanSignificant"))) %>% 
  mutate(term = as.factor(recode(term, "Temp_yearly_prev" = "PreviousYearTemp", "scale(Precip_yearly)" = "Precipitation", "Temp_yearly_spring" = "SpringTemp", "Temp_yearly_prev:scale(Precip_yearly)" = "PreviousYearTemp:Precipitation", "scale(Precip_yearly):Temp_yearly_spring" ="Precipitation:SpringTemp"))) %>% 
  mutate(term = factor(term, levels = c("PreviousYearTemp", "Precipitation", "SpringTemp", "PreviousYearTemp:Precipitation", "Precipitation:SpringTemp"))) %>% 
  mutate(Trait_trans = factor(Trait_trans, levels = c("Leaf_Area_cm2_log", "Plant_Height_mm_log", "Dry_Mass_g_log", "Wet_Mass_g_log", "C_percent", "SLA_cm2_g_log", "N_percent", "CN_ratio_log", "LDMC", "Leaf_Thickness_Ave_mm"))) %>%
  ggplot(aes(x = fct_rev(Trait_trans), y = effect, fill = coloring, color = moments)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6), width = 0.7) +
  #geom_bar_pattern(aes(pattern = 'stripe'), stat = "identity", position = "dodge") +
  #geom_errorbar(aes(xmin = effect-std.error, xmax = effect+std.error)) +
  facet_grid(~term, scales = "free") +
  scale_color_manual(values = c("#BAD8F7", "#89B7E1", "#2E75B6")) +
  scale_fill_manual(values = c("#EDEDED", "#BAD8F7", "#EDEDED", "#89B7E1", "#EDEDED", "#2E75B6")) +
  #scale_shape_manual(values = c(1,19)) +
  geom_hline(yintercept =  0) +
  theme_bw() +
  coord_flip() +
  guides(fill = "none", color = "none", size = "none") +
  theme(axis.title.y=element_blank())

#geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2, position=position_dodge(.9))


a <- c("Mean (Space)", "Mean (Time)", "Skewness", "Mean (Space)", "Mean (Time)", "Skewness")
b <- c(3, 2.8, 2.2, 1.5, 3.1, 2.8)
c <- c("Trait 1", "Trait 1","Trait 1", "Trait 2", "Trait 2", "Trait 2")
d <- data.frame(a,b, c)

nudge <- c(-1.7, -1.5, -0.9)

legend_significant_plot <- d %>% 
  filter(c == "Trait 1") %>% 
  mutate(a = factor(a, levels = c("Skewness", "Mean (Time)", "Mean (Space)"))) %>% 
ggplot(aes(x = a, y = b, fill = a, color = a)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.6), width = 1) +
  scale_fill_manual(values = c( "#BAD8F7", "#89B7E1", "#2E75B6")) +
  scale_color_manual(values = c( "#BAD8F7", "#89B7E1", "#2E75B6")) +
  theme_minimal() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text = element_blank(), plot.title = element_text(size=15, hjust = 0.5)) +
  guides(fill = "none", color = "none", size = "none") +
  geom_text(aes(x = a, y = b, label=a ), color="black", size=4, nudge_y = nudge) +
  coord_flip() +
  ggtitle("Significant")


legend_nonsignificant_plot <- d %>% 
  filter(c == "Trait 1") %>% 
  mutate(a = factor(a, levels = c("Skewness", "Mean (Time)", "Mean (Space)"))) %>% 
  ggplot(aes(x = a, y = b, fill = a, color = a)) +
  geom_bar(stat = "identity", position = position_dodge(width=1), width = 1) +
  scale_fill_manual(values = c( "#EDEDED", "#EDEDED", "#EDEDED")) +
  scale_color_manual(values = c( "#BAD8F7", "#89B7E1", "#2E75B6")) +
  theme_minimal() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text = element_blank(), plot.title = element_text(size=15, hjust = 0.5)) +
  guides(fill = "none", color = "none", size = "none") +
  geom_text(aes(x = a, y = b, label=a ), color="black", size=4, nudge_y = nudge) +
  coord_flip()+
  ggtitle("Non significant")
  


# Move to a new page
grid.newpage()
# Create layout : nrow = 3, ncol = 2
pushViewport(viewport(layout = grid.layout(nrow = 5, ncol = 6)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 
# Arrange the plots
print(mean_time_space_skewness_plot, vp = define_region(row = 1:4, col = 1:6))   # Span over two columns
print(legend_significant_plot, vp = define_region(row = 5, col = 3))
print(legend_nonsignificant_plot, vp = define_region(row = 5, col = 5))


sum_moments_climate_fullcommunity %>% 
  group_by(Site, turfID, Trait_trans, year) %>% 
  mutate(skewness = mean(skew),
         kurtosis = mean(kurt)) %>% 
  select(Site, turfID, Trait_trans, year, skewness, kurtosis, Temp_yearly_prev, Precip_yearly, Temp_yearly_spring, Temp_level) %>% 
  ggplot(aes(x = Temp_yearly_prev, y = kurtosis, color = Temp_level)) +
  geom_point() +
  facet_wrap(~Trait_trans, scales = "free")



mean_direction_shift <- model_output_space %>% 
  filter(moments == "mean") %>% 
  mutate(mean_direction_shift = ifelse(effect > 0, "positive", "negative")) %>%
  ungroup() %>% 
  select(Trait_trans, term, mean_direction_shift) 

skewness_direction_shift_time <- model_output_time %>% 
  filter(moments == "skewness") %>% 
  mutate(skewness_direction_shift_time = ifelse(effect > 0, "positive", "negative")) %>%
  ungroup() %>% 
  select(Trait_trans, term, skewness_direction_shift_time) 

### Directional change in skewness compared to the mean, and timely changes with skewness ###

model_output_space %>% 
  filter(moments == "skewness") %>% 
  left_join(mean_direction_shift, by = c("Trait_trans" = "Trait_trans", "term" = "term")) %>% 
  left_join(skewness_direction_shift_time, by = c("Trait_trans" = "Trait_trans", "term" = "term")) %>% 
  mutate(Trait_trans = factor(Trait_trans, levels = c("Leaf_Area_cm2_log", "Plant_Height_mm_log", "Dry_Mass_g_log", "Wet_Mass_g_log", "C_percent", "SLA_cm2_g_log", "N_percent", "CN_ratio_log", "LDMC", "Leaf_Thickness_Ave_mm"))) %>%
  filter(term %in% c("Temp_yearly_prev", "Temp_yearly_spring")) %>% 
  ggplot(aes(x = fct_rev(Trait_trans), y = effect, col = skewness_direction_shift_time)) +
  geom_point() +
  facet_wrap(term ~ mean_direction_shift, nrow = 2, scales = "free_x") +
  #geom_vline(xintercept =  0) +
  geom_hline(yintercept =  0) +
  coord_flip()


### Making boxplot of variance and kurtosis at temp and precip levels ####

moments_clim_long_fullcommunity %>% 
  filter(moments == "kurtosis") %>% 
  ggplot(aes(x = Precip_level, y = value, fill = Precip_level)) +
  geom_violin() +
  # stat_summary(fun.data=mean_sdl, 
  #              geom="pointrange", color="black") +
  geom_boxplot(width=0.1, fill="white") +
  facet_wrap(~Trait_trans, nrow = 4, scales = "free") +
  scale_fill_manual(values = Precip_palette) +
  ylab("Community weighted kurtosis")

sum_moments_climate_fullcommunity %>% 
  group_by(Site, blockID, turfID, Trait_trans, year) %>% 
  mutate(skewness = mean(skew),
         kurtosis = mean(kurt)) %>% 
  select(skewness, kurtosis, Site, turfID, year, Temp_level, Temp_level) %>% 
  unique() %>% 
  ggplot(aes(x = skewness^2, y = kurtosis, col= Temp_level)) +
  geom_point() +
  facet_wrap(~Trait_trans, nrow = 4)

moments_clim_long_fullcommunity %>% 
  filter(moments == "variance") %>% 
  ggplot(aes(x = Precip_level, y = value, fill = Precip_level)) +
  geom_violin() +
  stat_summary(fun.data=mean_sdl, 
               geom="pointrange", color="black") +
  facet_wrap(~Trait_trans, nrow = 4, scales = "free") +
  scale_fill_manual(values = Precip_palette)
  

#### Making plots for predicted values and observed values

Trend_SLA_plot <- ggplot(SLA_mean, aes(x = Precip_yearly, y = value)) +
  geom_point(color = "grey") +
  geom_line(aes(x = Precip_yearly, y = predicted, color = factor(Temp_yearly_spring)), data=newdata_SLA, size = 1, inherit.aes = FALSE, show.legend = TRUE) +
  scale_color_manual(values = Temp_palette) +
  labs(x = "", y = "", title = "Specific leaf area (cm2/g log)", color = "Spring temperature") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5))

Trend_SLA_time_plot <- ggplot(SLA_mean, aes(x = Precip_yearly, y = value)) +
  geom_point(color = "grey") +
  geom_line(aes(x = Precip_yearly, y = predicted, color = factor(Temp_yearly_prev)), data=newdata_time_SLA, size = 1, inherit.aes = FALSE, show.legend = TRUE) +
  scale_color_manual(values = Temp_palette) +
  labs(x = "", y = "", title = "Specific leaf area (cm2/g log)", color = "Spring temperature") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5))

Trend_LDMC_plot <- ggplot(LDMC_mean, aes(x = Precip_yearly, y = value)) +
  geom_point(color = "grey") +
  geom_line(aes(x = Precip_yearly, y = predicted, color = factor(Temp_yearly_spring)), data=newdata_LDMC, size = 1, inherit.aes = FALSE, show.legend = TRUE) +
  scale_color_manual(values = Temp_palette) +
  labs(x = "", y = "", title = "Leaf dry matter content (g/g)", color = "Spring temperature") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5))

Trend_LA_plot <- ggplot(Leaf_area_mean, aes(x = Precip_yearly, y = value)) +
  geom_point(color = "grey") +
  geom_line(aes(x = Precip_yearly, y = predicted, color = factor(Temp_yearly_spring)), data=newdata_LA, size = 1, inherit.aes = FALSE, show.legend = TRUE) +
  scale_color_manual(values = Temp_palette) +
  labs(x = "", y = "", title = "Leaf area (cm2 log)", color = "Spring temperature") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5))

Trend_C_plot <- ggplot(C_percent_mean, aes(x = Precip_yearly, y = value)) +
  geom_point(color = "grey") +
  geom_line(aes(x = Precip_yearly, y = predicted, color = factor(Temp_yearly_spring)), data=newdata_C, size = 1, inherit.aes = FALSE, show.legend = TRUE) +
  scale_color_manual(values = Temp_palette) +
  labs(x = "", y = "", title = "Carbon content of leaf (%)", color = "Spring temperature") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5))

Trend_figure <- ggarrange(Trend_SLA_plot, Trend_LDMC_plot, Trend_LA_plot, Trend_C_plot, nrow = 2, ncol = 2, common.legend = TRUE, legend = "right", labels = c("A)", "B)", "C)", "D)"))
Fig_TDT_paper <- annotate_figure(Trend_figure,
              bottom = text_grob("Annual precipitation (mm)", hjust = 0.7, size = 13))

ggsave(filename = "Trend_figure.png", plot = Fig_TDT_paper, width = 30, height = 19, units = "cm")
ggsave(filename = "Trend_figure_smaller.png", plot = Fig_TDT_paper, width = 24, height = 14, units = "cm")
