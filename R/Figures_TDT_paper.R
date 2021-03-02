#### Figures - Trait driver theory paper ####

## Libraries ##
library(ggpubr)
library(ggridges)
library(ggfortify)
library(corrplot)
library(svglite)
library(grid)
library(gridExtra)
library(maps)
library(mapdata)
library(grid)
library(ggvegan)

## Color palettes ##
Temp_palette <- c("#d8c593", "#dd7631", "#bb3b0e")
Precip_palette <- c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")

## Without intraspecific plot ##
# This code is to test if the without intra bootstrapping worked

# plot1 <- sum_moments_climate_without_intra %>% 
#   filter(Trait_trans == "SLA_cm2_g_log") %>% 
# ggplot(aes(x = Precip_yearly, y = mean, color = Temp_level))+
#   geom_point() +
#   geom_smooth(method = "lm") +
#   ylim(c(4.75, 5.75)) +
#   ggtitle("Without intraspecific")
# 
# plot2 <- sum_moments_climate_fullcommunity %>% 
#   filter(Trait_trans == "SLA_cm2_g_log") %>% 
#   ggplot(aes(x = Precip_yearly, y = mean, color = Temp_level))+
#   geom_point() +
#   geom_smooth(method = "lm") +
#   ylim(c(4.75, 5.75)) +
#   ggtitle("With intraspecific")
# 
# ggarrange(plot1, plot2, ncol = 2)

env_shift <- env %>% 
  group_by(Site) %>% 
  mutate(shift_temp = Temp_decade - Temp_century,
         shift_precip = Precip_decade - Precip_century) %>% 
  select(Site, shift_temp, shift_precip) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(mean_shift_temp = mean(shift_temp),
         sd_shift_temp = sd(shift_temp),
         mean_shift_precip = mean(shift_precip),
         sd_shift_precip = sd(shift_precip))


## Climate figure ##

env %>% 
  mutate(Temp_level = recode(Temp_level, "10.5" = "Boreal",
                           "8.5" = "Sub-alpine",
                           "6.5" = "Alpine")) %>% 
  ggplot(aes(x = Year, y = Temp_summer)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~Temp_level) +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Summer temperature (°C)") +
  xlab("Year") +
  scale_x_continuous(breaks = c(2009, 2011, 2013, 2015, 2017, 2019))

ggsave("Temperature_over_time.pdf", width = 20 , height = 11, units = "cm")
  

env %>% 
  mutate(Precip_level = recode(Precip_level, "600" = "Driest",
                               "1200" = "Dry",
                               "2000" = "Wet",
                               "2700" = "Wettest")) %>% 
  ggplot(aes(x = Year, y = Precip_yearly)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~Precip_level) +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Annual precipitation (m)") +
  xlab("Year") +
  scale_x_continuous(breaks = c(2009, 2011, 2013, 2015, 2017, 2019))

ggsave("Precipitation_over_time.png", width = 22 , height = 11, units = "cm")


climate <- env %>% 
  mutate(Temp_old = recode(Temp_level, "10.5" = "Boreal 1960-90",
                           "8.5" = "Sub-alpine 1960-90",
                           "6.5" = "Alpine 1960-90")) %>% 
  mutate(Temp_level = recode(Temp_level, "10.5" = "Boreal 2009-19",
                           "8.5" = "Sub-alpine 2009-19",
                           "6.5" = "Alpine 2009-19")) %>% 
  mutate(Precip_level = recode(Precip_level, "600" = "Driest",
                             "1200" = "Dry",
                             "2000" = "Wet",
                             "2700" = "Wettest")) %>% 
  mutate(Temp_old = factor(Temp_old, levels = c("Alpine 1960-90", "Boreal 1960-90", "Sub-alpine 1960-90"))) %>%
  mutate(Temp_level = factor(Temp_level, levels = c("Alpine 2009-19", "Boreal 2009-19", "Sub-alpine 2009-19"))) %>%
ggplot(aes(x = Precip_decade, y = Temp_decade,
           color = Precip_level, fill = Precip_level, shape = Temp_level)) +
  geom_point(aes(x = Precip_century, y = Temp_century, shape = Temp_old, size = 6)) +
  geom_segment(aes(x = Precip_decade, y = Temp_decade, yend = Temp_century, xend = Precip_century)) +
  geom_pointrange(aes(ymin = Temp_decade-Temp_se, ymax = Temp_decade+Temp_se)) +
  geom_errorbarh(aes(xmin = Precip_decade-Precip_se, xmax = Precip_decade+Precip_se)) +
  labs(x = "Annual precipitation in m", y = "Summer temperature in °C") +
  scale_color_manual(name = "Precipitation", values = c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")) +
  scale_fill_manual(name = "Precipitation", values = c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")) +
  scale_shape_manual(name = "Temperature", values = c(2, 24, 6, 25, 1, 21)) + 
  guides(fill = "none", size = "none", shape = "none", color = "none") +
  theme_bw(base_size = 15)


#ggsave("SeedClim_climate_over_time.jpg", width = 22 , height = 14, units = "cm")

#### Climate grid on map figure ####


dat <- read.table(header = TRUE, text = "
siteID           Latitude Longitude Temperature Precipitation
Alrust           60.8203    8.70466           2             1
Arhelleren       60.6652    6.33738           3             3
Fauske           61.0355    9.07876           3             1
Gudmedalen       60.8328    7.17561           1             3
Hogsete          60.876     7.17666           2             2
Lavisdalen       60.8231    7.27596           1             2
Ovstedal         60.6901    5.96487           3             4
Rambera          61.0866    6.63028           2             3
Skjellingahaugen 60.9335    6.41504           1             4
Ulvhaugen        61.0243    8.12343           1             1
Veskre           60.5445    6.51468           2             4
Vikesland        60.8803    7.16982           3             2
")

dat <- dat %>% 
  mutate(Precipitation = as.factor(Precipitation),
         Temperature = as.factor(Temperature))

precipLab <- c("Driest", "Dry", "Wet", "Wettest")
tempLab <- c("Alpine", "Sub-alpine", "Boreal")

xlim <- range(dat$Longitude) + c(-1, 0.5)
ylim <- range(dat$Latitude) + c(-0.5, 1)


norwaymap <- map_data("world", "Norway")
norwaymapHires <- map_data("worldHires", "Norway")


Scandinaviamap <- map_data("world", c("Norway", "Sweden", "Finland"))
norwaymapHires <- map_data("worldHires", "Norway")

#function for theme on plots

maptheme <- function(...) {
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 22),
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 18),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    ...
  )
}

Norway_map <- ggplot() +
  geom_map(data = Scandinaviamap, aes(x = long, y = lat, map_id = region, alpha = 0.6),
           map = Scandinaviamap, colour = "black", fill = "grey60") +
  geom_rect(data = data.frame(),
            aes(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2]),
            colour = "black", fill = "#76B4FF", alpha = 0.8) +
  coord_map(xlim = c(4, 32), ylim = c(55, 71)) +
  labs(x = NULL, y = NULL) +
  guides(alpha = FALSE) +
  maptheme()

Zoomed_in_map <- ggplot(dat, aes(x = Longitude, y = Latitude, fill = Precipitation, shape = Temperature)) +
  geom_map(aes(x = long, y = lat, map_id = region), data = norwaymapHires, map = norwaymapHires, color = NA, fill = "grey70", inherit.aes = FALSE) +
  geom_point(size = 7) +
  coord_map(xlim = xlim, ylim = ylim) +
  guides(shape = guide_legend(override.aes = list(fill = "grey70")),
         fill = guide_legend(override.aes = list(shape = 21))) +
  scale_shape_manual(values = c(24, 21, 25), labels = tempLab, breaks = 1:3) +
  scale_fill_manual(values = Precip_palette, labels = precipLab, breaks = 1:4) +
  maptheme()

## Code for saving the figure

library(patchwork)

plot <- Zoomed_in_map +
  inset_element(climate, left = 0.46, bottom = 0.55, right = 0.93, top = 1, align_to = "plot") +
  inset_element(Norway_map, left = 0.7, bottom = 0.07, right = 0.95, top = 0.39, align_to = "plot") +
  plot_layout(guides = 'collect', widths = 1, heights = 1) 

#ggsave(plot = plot, "SeedClim_climate_grid.pdf", width = 34, height = 22, units = "cm")


  # png("SeedClim_climate_grid.png", width = 1285, height = 861)
  # grid.newpage()
  # vp_zoomed_in_map <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the zoomed in map
  # vp_norway_map <- viewport(width = 0.4, height = 0.4, x = 0.685, y = 0.8)  # the inset in upper left of scandinacia
  # print(Zoomed_in_map, vp = vp_zoomed_in_map)
  # print(Norway_map, vp = vp_norway_map)
  # dev.off()

### Community weighted skewness over time in different temp levels ###

# moments_clim_long_fullcommunity %>% 
#   filter(Trait_trans == "SLA_cm2_g_log",
#          moments == "skewness") %>% 
#   mutate(year = as.factor(year)) %>% 
#   ggplot(aes(x = value, y = fct_rev(year), fill = fct_rev(as.factor(Temp_level)))) +
#   stat_density_ridges(quantile_lines = TRUE, quantiles = c(0.025, 0.5, 0.975), alpha = 0.7) +
#   geom_vline(xintercept =  0, linetype = 2, size = 1) +
#   facet_grid(~Temp_level) +
#   theme_ridges() + 
#   theme(legend.position = "none") +
#   labs(x = "SLA (cm2/g)", title = "Probability distribution skewness av SLA", y = "Year") +
#   theme_bw(base_size = 12) +
#   scale_fill_manual(name = "Temperature level", values = Temp_palette, labels = c("Boreal", "Sub-alpine", "Alpine"))
# 
# ggsave("SLA_moments_over_time.jpg", width = 30 , height = 20, units = "cm")

#### Correlation plot ####

# Correlations 

corr <- round(cor(Corr_traits), 1) 
head(corr[, 1:6])

# P-values 

p.mat <- cor_pmat(Corr_traits)
#head(p.mat_17[, 1:4])

# Correlation plot

ggcorrplot(corr, hc.order = FALSE,
           type = "lower", lab = TRUE,)

#ggsave("Correlation_plot.jpg", width = 30 , height = 20, units = "cm")

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

# fviz_pca_var(pca_trait,
#              col.var = "contrib", # Color by contributions to the PC
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )

#### Ordination with traits ####

Ord_plot_traits <- fviz_pca_biplot(pca_trait, repel = TRUE,
                col.var = "#2A2A2A", # Variables color
                col.ind = "#AAB7B8",
                label = "var",
                labelsize = 5) +
  theme_minimal(base_size = 14) +
  guides(shape = FALSE) +
  #scale_color_manual(name = "Summer temperature", values = c("#8DD5E1", "#FCB971", "#B93B3B")) +
  coord_fixed() +
  labs(title = "") +
  xlab("PCA1 (44.1%)") +
  ylab("PCA2 (19.5%)") +
  theme(plot.title = element_text(hjust = 0.1))

#ggsave("Ordination_LES_Size.svg", width = 22 , height = 16, units = "cm", dpi = 600)

#### Ordination with temperature ####


Ord_plot_temp <- fviz_pca_ind(pca_trait, repel = TRUE,
                col.var = "#2A2A2A", # Variables color
                label = "none",
                labelsize = 5, 
                habillage = Ord_boot_traits$Temp_level, 
                addEllipses = TRUE,
                ellipse.level = 0.95,
                palette = c("#d8c593", "#dd7631", "#bb3b0e")) +
  theme_minimal(base_size = 14) +
  #scale_color_manual(name = "Summer temperature", values = c("#8DD5E1", "#FCB971", "#B93B3B")) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_blank(), axis.title = element_blank()) +
  labs(title = "", fill = "Summer temperature", color = "Summer temperature", shape = "Summer temperature") +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.1))

#ggsave("Ordination_Temp.svg", width = 18 , height = 11, units = "cm", dpi = 600)


### Ordination with precipitation ####

Ord_plot_precip <- fviz_pca_ind(pca_trait, repel = TRUE,
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
  labs(title = "", fill = "Yearly precipitation", color = "Yearly precipitation", shape = "Yearly precipitation") +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.1))

#ggsave("Ordination_Precip.svg", width = 18 , height = 11, units = "cm", dpi = 600)
  
## Ordination over time ##
  
  
pca_fort <- fortify(pca_trait, display = "Sites") %>% 
  bind_cols(Ord_boot_traits[1:6]) %>% 
  filter(year %in% c(2009, 2019)) 
  

Ord_plot_time <- pca_fort %>% 
ggplot(aes(x = PC1, y = PC2, colour = Temp_level, group = turfID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2009, 1, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_color_manual(name = "Summer temperature", values = Temp_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1), axis.title = element_blank()) +
  labs(fill = "Yearly precipitation", color = "Yearly precipitation", shape = "Yearly precipitation") +
  guides(color = FALSE) 


d <- ggarrange(Ord_plot_traits, 
               ggarrange(Ord_plot_precip, Ord_plot_temp,  Ord_plot_time, ncol = 3, nrow = 1, labels = c("B", "C", "D"),legend = "none"), 
               nrow = 2, ncol = 1,
               labels = "A",
               widths = c(6, 2), 
               heights = c(2, 1))


c <- ggarrange(Ord_plot_traits, Ord_plot_time, Ord_plot_precip, Ord_plot_temp,   
               nrow = 2, ncol = 2,
               labels = c("A","B", "C", "D"),
               legend = "none")

#ggsave(plot = d, "Ord_timemean_temp_prec_new.jpg", width = 28 , height = 20, units = "cm")

#ggsave(plot = c, "Ord_timemean_temp_prec_four.jpg", width = 28 , height = 20, units = "cm")


### Figures with model output ###

#Table with model output

# write.table(<- model_output_space %>% 
#                filter(moments %in% c("mean", "skewness")) %>% 
#                select(-CIhigh.fit, -CIlow.fit, -Trend) %>% 
#                mutate(effect = round(effect, digits = 3),
#                       R2_marginal = round(R2_marginal, digits =3),
#                       R2_conditional = round(R2_conditional, digits = 3),
#                       std.error = round(std.error, digits = 3),
#                       staticstic = round(staticstic, digits = 3),
#                       df = round(df, digits = 3)),
#   file = "model_output_space.csv")

time_mixed <- model_output_time_mixed %>% 
  filter(moments == "mean") %>% 
  dplyr::select(Trait_trans, moments, term, effect, std.error, p.value) %>% 
  mutate(moments = "time")
  #filter(Trait_trans %in% c("SLA_cm2_g_log", "N_percent", "CN_ratio_log", "LDMC", "Leaf_Thickness_Ave_mm"))

# time_linear <- model_output_time_linear %>% 
#   filter(moments == "mean") %>% 
#   select(Trait_trans, moments, term, effect, std.error, p.value) %>% 
#   mutate(moments = "time")


## Mean mixed effect model ##

model_output_space_mixed %>% 
  ungroup() %>% 
  #filter(Trait_trans %in% c("Leaf_Area_cm2_log", "Plant_Height_mm_log", "Dry_Mass_g_log", "Wet_Mass_g_log", "C_percent")) %>% 
  bind_rows(time_mixed) %>%
  #bind_rows(without_intra) %>% 
  mutate(Trait_moment = paste0(moments, "_", Trait_trans)) %>% 
  filter(moments %in% c("mean", "time")) %>% 
  mutate(moments = factor(moments, levels = c("time", "mean"))) %>% 
  mutate(term = as.factor(recode(term, "scale(Precip_yearly)" = "Precipitation", "Temp_yearly_spring" = "SummerTemp", "Temp_yearly_spring:scale(Precip_yearly)" = "SummerTemp:Precipitation"))) %>% 
  mutate(Trait_trans = factor(Trait_trans, levels = c("Leaf_Area_cm2_log", "Plant_Height_mm_log", "Dry_Mass_g_log", "Wet_Mass_g_log", "C_percent", "SLA_cm2_g_log", "N_percent", "CN_ratio_log", "LDMC", "Leaf_Thickness_Ave_mm"))) %>%
  ggplot(aes(x = fct_rev(Trait_trans), y = effect, fill = term, color = moments)) +
  geom_bar(aes(alpha = rev(p.value)), stat = "identity", position = position_dodge(width=0.7), width = 0.7) +
  #geom_point(aes(size = if_else(p.value <0.05, 0.3, NA_real_)), position = position_dodge(width=0.6), show.legend = FALSE) +
  #geom_bar_pattern(aes(pattern = 'stripe'), stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = effect-std.error, ymax = effect+std.error), position = position_dodge(width=0.7), width = 0.3) +
  facet_grid(~term, scales = "free") +
  scale_fill_manual(values = c("#2E75B6", "#bb3b0e", "#9A86A9")) +
  # scale_fill_gradient(low = "#213964", ##2E75B6
  #                     high = "#BAD8F7",
  #                     guide = "colourbar") +
  scale_color_manual(values = c("black", "black")) +
  scale_alpha_continuous(range = c(0.1, 3)) +
  geom_hline(yintercept =  0) +
  theme_bw(base_size = 18) +
  coord_flip() +
  #guides(fill = "none", color = "none", size = "none") +
  theme(axis.title.y=element_blank()) +
  guides(color = FALSE, alpha = FALSE, fill = FALSE)

ggsave(filename = "trends_over_time_free_scales.pdf",  width = 34, height = 23, units = "cm")

model_output_space_mixed %>% 
  ungroup() %>% 
  #filter(Trait_trans %in% c("Leaf_Area_cm2_log", "Plant_Height_mm_log", "Dry_Mass_g_log", "Wet_Mass_g_log", "C_percent")) %>% 
  bind_rows(time_mixed) %>%
  #bind_rows(without_intra) %>% 
  mutate(Trait_moment = paste0(moments, "_", Trait_trans)) %>% 
  filter(moments %in% c("mean", "time")) %>% 
  mutate(moments = factor(moments, levels = c("time", "mean"))) %>% 
  mutate(term = as.factor(recode(term, "scale(Precip_yearly)" = "Precipitation", "Temp_yearly_spring" = "SummerTemp", "Temp_yearly_spring:scale(Precip_yearly)" = "SummerTemp:Precipitation"))) %>% 
  mutate(Trait_trans = factor(Trait_trans, levels = c("Leaf_Area_cm2_log", "Plant_Height_mm_log", "Dry_Mass_g_log", "Wet_Mass_g_log", "C_percent", "SLA_cm2_g_log", "N_percent", "CN_ratio_log", "LDMC", "Leaf_Thickness_Ave_mm"))) %>%
  ggplot(aes(x = fct_rev(Trait_trans), y = effect, fill = term, color = moments)) +
  geom_bar(aes(alpha = rev(p.value)), stat = "identity", position = position_dodge(width=0.7), width = 0.7) +
  #geom_point(aes(size = if_else(p.value <0.05, 0.3, NA_real_)), position = position_dodge(width=0.6), show.legend = FALSE) +
  #geom_bar_pattern(aes(pattern = 'stripe'), stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = effect-std.error, ymax = effect+std.error), position = position_dodge(width=0.7), width = 0.3) +
  facet_grid(~term, scales = "free") +
  scale_fill_manual(values = c("#2E75B6", "#bb3b0e", "#9A86A9")) +
  # scale_fill_gradient(low = "#213964", ##2E75B6
  #                     high = "#BAD8F7",
  #                     guide = "colourbar") +
  scale_color_manual(values = c("black", "black")) +
  scale_alpha_continuous(range = c(0.1, 3)) +
  geom_hline(yintercept =  0) +
  theme_bw(base_size = 18) +
  coord_flip() +
  #guides(fill = "none", color = "none", size = "none") +
  theme(axis.title.y=element_blank()) +
  guides(color = FALSE, alpha = FALSE, fill = FALSE)

#ggsave(filename = "trends_over_time_free_scales.pdf",  width = 27, height = 23, units = "cm")

## Community model output figure ##

time_com_mixed <- model_output_com_time_mixed %>% 
  dplyr::select(community_properties, term, estimate, std.error, p.value) %>% 
  mutate(moments = "time")

model_output_com_space_mixed %>% 
  mutate(moments = "space") %>% 
  bind_rows(time_com_mixed) %>%
  mutate(term = as.factor(recode(term, "Precip_yearly" = "Precipitation", "Temp_yearly_spring" = "SummerTemp", "Temp_yearly_spring:Precip_yearly" = "SummerTemp:Precipitation"))) %>% 
  mutate(community_properties = as.factor(recode(community_properties, "species_richness" = "Species richness", "vegetation_height" = "Vegetation height", "total_vascular" = "Total vascular plant cover", "total_bryophytes" = "Bryophyte cover", "other_cover" = "Other plants cover", "moss_height" = "Bryophyte depth", "graminoid_cover" = "Graminoid cover", "forb_cover" = "Forb cover"))) %>% 
  mutate(community_properties, factor(community_properties, levels = c("Other plants cover", "Bryophyte cover", "Graminoid cover", "Forb cover", "Total vascular plant cover", "Bryophyte depth", "Vegetation height","Species richness"))) %>% 
  mutate(moments = factor(moments, levels = c("time", "space"))) %>% 
  ggplot(aes(x = rev(community_properties), y = estimate, fill = term, color = moments)) +
  geom_bar(aes(alpha = rev(p.value)), stat = "identity", position = position_dodge(width=0.7), width = 0.7) +
  #geom_point(aes(size = if_else(p.value <0.05, 0.3, NA_real_)), position = position_dodge(width=0.6), show.legend = FALSE) +
  #geom_bar_pattern(aes(pattern = 'stripe'), stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = estimate-std.error, ymax = estimate+std.error), position = position_dodge(width=0.7), width = 0.2) +
  facet_grid(~term) +
  scale_fill_manual(values = c("#2E75B6", "#bb3b0e", "#9A86A9")) +
  # scale_fill_gradient(low = "#213964", ##2E75B6
  #                     high = "#BAD8F7",
  #                     guide = "colourbar") +
  scale_color_manual(values = c("black", "black")) +
  scale_alpha_continuous(range = c(0.005, 1.5)) +
  geom_hline(yintercept =  0) +
  theme_bw(base_size = 18) +
  coord_flip() +
  #guides(fill = "none", color = "none", size = "none") +
  theme(axis.title.y=element_blank()) +
  guides(color = FALSE, alpha = FALSE, fill = FALSE)

#ggsave(filename = "community_timeandspace.png",  width = 27, height = 17, units = "cm")

## Mean linear model ##

# model_output_space_linear %>% 
#   bind_rows(time_linear) %>%
#   #bind_rows(without_intra) %>% 
#   mutate(Trait_moment = paste0(moments, "_", Trait_trans)) %>% 
#   mutate(trend_col = ifelse(effect > 0, "positive", "negative")) %>% 
#   mutate(significance = ifelse(p.value > 0.05, "Non significant", "Significant")) %>% 
#   filter(moments %in% c("mean", "time")) %>% 
#   mutate(moments = factor(moments, levels = c("time", "mean"))) %>% 
#   #mutate(coloring = paste0(moments, "", significance)) %>% 
#   #mutate(coloring = factor(coloring, levels = c("timeNon significant", "timeSignificant", "meanNon significant", "meanSignificant" ))) %>% 
#   mutate(term = as.factor(recode(term, "Precip_yearly" = "Precipitation", "Temp_yearly_spring" = "SpringTemp", "Temp_yearly_spring:Precip_yearly" = "SpringTemp:Precipitation"))) %>% 
#   mutate(coloring = paste0(moments, "", term)) %>% 
#   mutate(coloring = factor(coloring, levels = c("timePrecipitation","meanPrecipitation","timeSpringTemp", "meanSpringTemp", "timeSpringTemp:Precipitation", "meanSpringTemp:Precipitation"))) %>% 
#   mutate(Trait_trans = factor(Trait_trans, levels = c("Leaf_Area_cm2_log", "Plant_Height_mm_log", "Dry_Mass_g_log", "Wet_Mass_g_log", "C_percent", "SLA_cm2_g_log", "N_percent", "CN_ratio_log", "LDMC", "Leaf_Thickness_Ave_mm"))) %>%
#   ggplot(aes(x = fct_rev(Trait_trans), y = effect, fill = coloring)) +
#   geom_bar(stat = "identity", position = position_dodge(width=0.6), width = 0.7) +
#   geom_point(aes(size = if_else(p.value <0.05, 0.3, NA_real_)), position = position_dodge(width=0.6), show.legend = FALSE) +
#   #geom_bar_pattern(aes(pattern = 'stripe'), stat = "identity", position = "dodge") +
#   #geom_errorbar(aes(xmin = effect-std.error, xmax = effect+std.error)) +
#   facet_grid(~term, scales = "free") +
#   scale_fill_manual(values = c("#BAD8F7", "#2E75B6", "#d8c593","#dd7631", "#D8C4E8", "#9A86A9")) +
#   #scale_shape_manual(values = c(1,19)) +
#   geom_hline(yintercept =  0) +
#   theme_bw() +
#   coord_flip() +
#   #guides(fill = "none", color = "none", size = "none") +
#   theme(axis.title.y=element_blank()) +
#   guides(fill = FALSE)

### Mean compared to skewness figures ###

heatmap_data <- SLA_mean_pred_heatmap %>% 
  bind_rows(SLA_skew_pred_heatmap) %>% 
  bind_rows(CN_ratio_mean_pred_heatmap) %>% 
  bind_rows(CN_ratio_skew_pred_heatmap) %>% 
  bind_rows(LA_mean_pred_heatmap) %>% 
  bind_rows(LA_skew_pred_heatmap) %>% 
  bind_rows(C_mean_pred_heatmap) %>% 
  bind_rows(C_skew_pred_heatmap)

limits <- range(heatmap_data$predicted)

heatmap_data %>% 
  group_by(trait, moment, Precip_yearly, Temp_yearly_spring) %>% 
  mutate(predicted = mean(predicted)) %>% 
  select(-year) %>% 
  unique() %>% 
  ggplot(aes(as.factor(Precip_yearly), as.factor(Temp_yearly_spring), fill = predicted)) +
  geom_tile() +
  scale_fill_gradient(low = "#DFD3EA", high = "#9B87AA") +
  facet_grid(trait ~ moment, scales = "free")

heatplot <-function(dat) {
  
  plot <- dat %>%
    group_by(Precip_yearly, Temp_yearly_spring) %>% 
    mutate(predicted = mean(predicted)) %>% 
    select(-year) %>% 
    unique() %>% 
    ggplot(aes(as.factor(Precip_yearly), as.factor(Temp_yearly_spring), fill = predicted)) +
    geom_tile() +
    scale_fill_gradient(low = "#DFD3EA", high = "#9B87AA") +
    xlab(label = "") +
    ylab(label = "") +
    theme_minimal()
  
  return(plot)
}

# scale_fill_gradient2(name= "logit(cover %)",low = "purple", mid = "white", high = "forest green", 
#                      midpoint = mean(bryocov$estimate))+
  

heatplot_skewness <-function(dat) {
  
  plot <- dat %>%
    group_by(Precip_yearly, Temp_yearly_spring) %>% 
    mutate(predicted = mean(predicted)) %>% 
    select(-year) %>% 
    unique() %>% 
    ggplot(aes(as.factor(Precip_yearly), as.factor(Temp_yearly_spring), fill = predicted)) +
    geom_tile() +
    scale_fill_gradient2(low ="#8C8C8C", mid = "#FFFFFF", high = "#9B87AA", midpoint = 0) +
    xlab(label = "") +
    ylab(label = "") +
    theme_minimal()
  
  return(plot)
}

SLA_mean_heatplot <- heatplot(SLA_mean_pred_heatmap)
SLA_skew_heatplot <- heatplot_skewness(SLA_skew_pred_heatmap)

CN_mean_heatplot <- heatplot(CN_ratio_mean_pred_heatmap)
CN_skew_heatplot <- heatplot_skewness(CN_ratio_skew_pred_heatmap)

LA_mean_heatplot <- heatplot(LA_mean_pred_heatmap)
LA_skew_heatplot <- heatplot_skewness(LA_skew_pred_heatmap)

C_mean_heatplot <- heatplot(C_mean_pred_heatmap)
C_skew_heatplot <- heatplot_skewness(C_skew_pred_heatmap)

heatplot <- ggarrange(SLA_mean_heatplot, SLA_skew_heatplot, CN_mean_heatplot, CN_skew_heatplot, LA_mean_heatplot, LA_skew_heatplot, C_mean_heatplot, C_skew_heatplot, nrow = 4, ncol = 2, labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)"), legend = "bottom")

ggsave(plot = heatplot, filename = "mean_skewness_heatplot.png",  width = 20, height = 29, units = "cm")

#### Making plots for predicted values and observed values

plot_predictions <-function(dat, trait, moment, newdata) {
  
  dat2 <- dat %>%
    filter(Trait_trans == trait,
           moments == moment,
           n == 75) %>% 
    unnest(data) %>% 
    ungroup()
  
  plot <- ggplot(dat2, aes(x = Temp_yearly_spring, y = value)) +
    geom_point(color = "grey") +
    geom_line(aes(x = Temp_yearly_spring, y = predicted, color = factor(Precip_yearly)), data=newdata, size = 1, inherit.aes = FALSE, show.legend = TRUE) +
    scale_color_manual(values = Precip_palette) +
    theme_minimal(base_size = 15) 

  return(plot)
}

SLA_mean_plot <- plot_predictions(memodel_data_fullcommunity_nottransformed, "SLA_cm2_g_log", "mean", SLA_mean_pred) +
  labs(y = "SLA (cm2/g log)", x = "", title = "Mean") +
  theme(plot.title = element_text(hjust = 0.5))
SLA_skew_plot <- plot_predictions(memodel_data_fullcommunity_nottransformed, "SLA_cm2_g_log", "skewness", SLA_skew_pred) + 
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  labs(y = "", x = "", title = "Skewness") +
  theme(plot.title = element_text(hjust = 0.5))

CN_ratio_mean_plot <- plot_predictions(memodel_data_fullcommunity_nottransformed, "CN_ratio_log", "mean", CN_ratio_mean_pred) +
  labs(y = "C/N ratio (log))", x = "") 
CN_ratio_skew_plot <- plot_predictions(memodel_data_fullcommunity_nottransformed, "CN_ratio_log", "skewness", CN_ratio_skew_pred) + geom_hline(yintercept = 0, color = "black", linetype = 2) +
  labs(y = "", x = "") 

LA_mean_plot <- plot_predictions(memodel_data_fullcommunity_nottransformed, "Leaf_Area_cm2_log", "mean", LA_mean_pred) +
  labs(y = "Leaf area (cm2 log)", x = "") 
LA_skew_plot <- plot_predictions(memodel_data_fullcommunity_nottransformed, "Leaf_Area_cm2_log", "skewness", LA_skew_pred)+ geom_hline(yintercept = 0, color = "black", linetype = 2) +
  labs(y = "", x = "") 

C_mean_plot <- plot_predictions(memodel_data_fullcommunity_nottransformed, "C_percent", "mean", C_mean_pred) +
  labs(y = "Carbon %", x = "") 
C_skew_plot <- plot_predictions(memodel_data_fullcommunity_nottransformed, "C_percent", "skewness", C_skew_pred) + geom_hline(yintercept = 0, color = "black", linetype = 2) +
  labs(y = "", x = "") 

figure <- ggarrange(SLA_mean_plot, SLA_skew_plot, CN_ratio_mean_plot, CN_ratio_skew_plot, LA_mean_plot, LA_skew_plot, C_mean_plot, C_skew_plot, nrow = 4, ncol = 2, labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)"), common.legend = TRUE, legend = "bottom")

ggsave(plot = figure, filename = "mean_skewness_figure.png",  width = 20, height = 29, units = "cm")
