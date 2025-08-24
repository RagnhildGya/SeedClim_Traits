#### Figures - Trait driver theory paper ####

#### Libraries ####
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
#library(ggvegan)
library(ggnewscale)
library(gghighlight)
library(merTools)
library(patchwork)
library(conflicted)
library(broom)
library(ggpattern)

#### Setting conflict standards ####

conflict_prefer("select", winner = "dplyr")
conflict_prefer("corrpot", winner = "corrplot")
conflict_prefer("arm", winner = "corrplot")
conflict_prefer("filter", winner = "dplyr")

#### Color palettes ####
Temp_palette <- c("#d8c593", "#dd7631", "#bb3b0e")
Precip_palette <- c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")

#### Climate figure ####

## Make the variables for plotting the shift in climate

env_shift <- env %>% 
  group_by(siteID) %>% 
  mutate(shift_temp = Temp_decade - Temp_century,
         shift_precip = Precip_decade - Precip_century) %>% 
  dplyr::select(siteID, shift_temp, shift_precip) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(mean_shift_temp = mean(shift_temp),
         sd_shift_temp = sd(shift_temp),
         mean_shift_precip = mean(shift_precip),
         sd_shift_precip = sd(shift_precip))


Temp_figure <- env %>% 
  mutate(Temp_level = recode(Temp_level, "10.5" = "Boreal",
                             "8.5" = "Sub-alpine",
                             "6.5" = "Alpine")) %>% 
  ggplot(aes(x = year, y = Temp_yearly_spring)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~Temp_level) +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Summer temperature (°C)") +
  xlab("Year") +
  scale_x_continuous(breaks = c(2009, 2011, 2013, 2015, 2017, 2019))

#ggsave(plot = Temp_figure, "Temperature_over_time.pdf", width = 20 , height = 11, units = "cm")


Precip_figure <- env %>% 
  mutate(Precip_level = recode(Precip_level, "600" = "Driest",
                               "1200" = "Dry",
                               "2000" = "Wet",
                               "2700" = "Wettest")) %>% 
  ggplot(aes(x = year, y = Precip_yearly)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~Precip_level) +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Annual precipitation (m)") +
  xlab("Year") +
  scale_x_continuous(breaks = c(2009, 2011, 2013, 2015, 2017, 2019))

#ggsave(plot = Precip_figure, "Precipitation_over_time.pdf", width = 22 , height = 11, units = "cm")


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
  ggplot(aes(x = Precip_decade, y = Temp_decade_figure,
             color = Precip_level, fill = Precip_level, shape = Temp_level)) +
  geom_point(aes(x = Precip_century, y = Temp_century, shape = Temp_old, size = 10)) +
  geom_segment(aes(x = Precip_decade, y = Temp_decade_figure, yend = Temp_century, xend = Precip_century, size = 0.5)) +
  geom_pointrange(aes(ymin = Temp_decade_figure-Temp_se_figure, ymax = Temp_decade_figure+Temp_se_figure, size = 0.5)) +
  geom_errorbarh(aes(xmin = Precip_decade-Precip_se, xmax = Precip_decade+Precip_se, size = 0.5)) +
  labs(x = "Annual precipitation in m", y = "Summer temperature in °C") +
  scale_color_manual(name = "Precipitation", values = c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")) +
  scale_fill_manual(name = "Precipitation", values = c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")) +
  scale_shape_manual(name = "Temperature", values = c(2, 6, 1, 24, 25, 21)) + 
  guides(fill = "none", size = "none", shape = "none", color = "none") +
  theme_bw(base_size = 18)


#ggsave("SeedClim_climate_over_time.jpg", width = 22 , height = 14, units = "cm")

#### Climate grid on map figure ####

#Making table with coordinates for the sites

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
ylim <- range(dat$Latitude) + c(-0.3, 1.3)

#Get maps
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

plot <- Zoomed_in_map +
  inset_element(climate, left = 0.18, bottom = 0.55, right = 0.65, top = 1, align_to = "plot") +
  inset_element(Norway_map, left = 0.595, bottom = 0.55, right = 0.96, top = 1, align_to = "plot") +
  plot_layout(guides = 'collect', widths = 1, heights = 1) 

#ggsave(plot = plot, "SeedClim_climate_grid2.pdf", width = 34, height = 22, units = "cm")


#### Correlation plot ####

# Correlations 

Trait_climate_corr <- Corr_traits %>%
  select(-Temp_yearly_prev, -Temp_summer, -Precip_yearly_spring) %>%
  rename(Temp_summer = Temp_yearly_spring)

Climate_corr <- Corr_traits %>%
  select(Temp_yearly_prev, Temp_summer, Precip_yearly_spring, Precip_yearly, Temp_yearly_spring) %>%
  rename(Temp_summer_MaySeptember = Temp_summer,
         Temp_summer_MayJuly = Temp_yearly_spring)

corr <- round(cor(Trait_climate_corr), 2)
corr1 <- round(cor(Climate_corr, use = "complete.obs"), 2)

# P-values
p.mat <- cor_pmat(Corr_traits)
#head(p.mat_17[, 1:4])

# Correlation plot

# New order
new_order_traits <- c("Leaf_Area_cm2_log", "C_percent", "Dry_Mass_g_log", 
                      "Wet_Mass_g_log", "Plant_Height_mm_log",
                      "LDMC", "CN_ratio", "N_percent", 
                      "Leaf_Thickness_Ave_mm", "SLA_cm2_g", 
                      "Temp_summer", "Precip_yearly")

new_order_climate <- c("Temp_summer_MaySeptember", "Temp_summer_MayJuly", 
                       "Temp_yearly_prev", 
                      "Precip_yearly", "Precip_yearly_spring")

# Pretty names for plotting
new_names_traits <- c("Leaf Area", "Leaf C (%)", "Leaf Dry Mass", 
                      "Leaf Wet Mass", "Plant Height",
                      "LDMC", "Leaf C/N", "Leaf N (%)", 
                      "Leaf Thickness", "SLA", 
                      "Summer temperature", "Annual precipitation")

new_names_climate <- c("Summer temperature (May-September, tetraterm)", "Summer temperature (May-July)", 
                      "Summer temperature previous year (tetraterm)",
                      "Annual precipitation", 
                      "Summer precipitation")


# Reorder and subset the correlation matrix
corr_ordered <- corr[new_order_traits, new_order_traits]
corr_ordered <- corr[new_order_traits, new_order_traits]

corr_ordered_climate <- corr1[new_order_climate, new_order_climate]
corr_ordered_climate <- corr1[new_order_climate, new_order_climate]

# Rename the row and column names
rownames(corr_ordered) <- new_names_traits
colnames(corr_ordered) <- new_names_traits

rownames(corr_ordered_climate) <- new_names_climate
colnames(corr_ordered_climate) <- new_names_climate

# Plot
cor_traits <- ggcorrplot(corr_ordered, 
                         hc.order = FALSE, 
                         type = "lower", 
                         lab = TRUE)

#ggsave(plot = cor_traits, "Correlation_plot.pdf", width = 22 , height = 16, units = "cm")

cor_climate <- ggcorrplot(corr_ordered_climate, 
                          hc.order = FALSE,
                          type = "lower",
                          lab = TRUE)

#ggsave(plot = cor_climate, "Correlation_plot_climate.pdf", width = 20 , height = 15, units = "cm")


#### Ordination ####

contr_traits <- as.data.frame(var$cos2) %>% 
  select(Dim.1, Dim.2) %>% 
  rownames_to_column("trait") %>% 
  group_by(trait) %>% 
  mutate(contribution = sum(Dim.1, Dim.2))

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
  guides(shape = "none") +
  #scale_color_manual(name = "Summer temperature", values = c("#8DD5E1", "#FCB971", "#B93B3B")) +
  coord_fixed() +
  labs(title = "") +
  xlab("PCA1 (43.5%)") + #Numbers added manually from the fviz_eig plot above
  ylab("PCA2 (20.5%)") + #Numbers added manually from the fviz_eig plot above
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
                              palette = Temp_palette) +
  theme_minimal(base_size = 14) +
  #scale_color_manual(name = "Summer temperature", values = c("#8DD5E1", "#FCB971", "#B93B3B")) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_blank()) +
  labs(title = "", fill = "Summer temperature", color = "Summer temperature", shape = "Summer temperature") +
  xlab("PCA1 (43.5%)") + #Numbers added manually from the fviz_eig plot above
  ylab("PCA2 (20.5%)") + #Numbers added manually from the fviz_eig plot above
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.1),legend.position = 'top')

#ggsave("Ordination_Temp.svg", width = 18 , height = 11, units = "cm", dpi = 600)


### Ordination with precipitation ####

Ord_plot_precip <- fviz_pca_ind(pca_trait, repel = TRUE,
                                col.var = "#2A2A2A", # Variables color
                                label = "none",
                                labelsize = 5, 
                                habillage = Ord_boot_traits$Precip_level, 
                                addEllipses = TRUE,
                                ellipse.level = 0.95,
                                palette = Precip_palette) +
  theme_minimal(base_size = 15) +
  #scale_color_manual(name = "Summer temperature", values = c("#8DD5E1", "#FCB971", "#B93B3B")) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_blank()) +
  labs(title = "", fill = "Yearly precipitation", color = "Yearly precipitation", shape = "Yearly precipitation") +
  xlab("PCA1 (43.5%)") + #Numbers added manually from the fviz_eig plot above
  ylab("PCA2 (20.5%)") + #Numbers added manually from the fviz_eig plot above
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.1), legend.position = 'top')

#ggsave("Ordination_Precip.svg", width = 18 , height = 11, units = "cm", dpi = 600)

### Ordination over time ###

pca_fort <- augment(pca_trait, display = "Sites") %>% 
  bind_cols(Ord_boot_traits[1:6]) %>% 
  filter(year %in% c(2009, 2019)) 


Ord_plot_time <- pca_fort %>% 
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = Temp_level, group = turfID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2009, 5, NA_real_), shape = Temp_level), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_color_manual(name = "Summer temperature", values = Temp_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) +
  labs(fill = "Yearly precipitation", color = "Yearly precipitation", shape = "Yearly precipitation") +
  xlab("PCA1 (43.5%)") + #Numbers added manually from the fviz_eig plot above
  ylab("PCA2 (20.5%)") + #Numbers added manually from the fviz_eig plot above
  guides(color = "none") +
  scale_shape_manual(values = c(16, 17, 15))


c <- ggarrange(Ord_plot_traits, Ord_plot_time, Ord_plot_precip, Ord_plot_temp,   
               nrow = 2, ncol = 2,
               labels = c("A)","B)", "C)", "D)"),
               legend = "none")

#ggsave(plot = c, "Ord_timemean_temp_prec_four.pdf", width = 26 , height = 18, units = "cm")


### Ordination - RDA ###

## Plot the RDA

# Get site and biplot scores
site_scores <- scores(RDA_multi_year, display = "sites", scaling = 2)
arrow_scores <- scores(RDA_multi_year, display = "bp", scaling = 2)
trait_scores <- scores(RDA_multi_year, display = "species", scaling = 2)

# Convert to data frames
site_df <- as.data.frame(site_scores)
site_df$site <- rownames(site_df)

arrow_df <- as.data.frame(arrow_scores)
arrow_df$var <- rownames(arrow_df)

trait_df <- as.data.frame(trait_scores)
trait_df$var <- rownames(trait_df)

# Create the plot
ggplot() +
  geom_point(data = site_df, aes(x = RDA1, y = RDA2), color = "grey") + 
  geom_segment(data = trait_df,
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.2, "cm")),
               linewidth = 1) +
  geom_text(data = trait_df,
            aes(x = RDA1, y = RDA2, label = var),
            hjust = -0.1, vjust = -0.1, size = 5) +
  theme_minimal() +
  labs(x = "RDA1", y = "RDA2", color = "Variable group") +
  theme(legend.position = "bottom")




newdat <- expand.grid(
  Temp_level = levels(Ord_boot_traits$Temp_level),
  Precip_level = levels(Ord_boot_traits$Precip_level),
  year = c(2009, 2019)  # your first and last year
)

#Predict site scores
# Get predicted site scores in RDA space
pred_scores <- predict(RDA_multi_year, newdata = newdat, type = "lc", scaling = 2)
pred_df <- as.data.frame(pred_scores)

# Add metadata
pred_df$Temp_level <- newdat$Temp_level
pred_df$Precip_level <- newdat$Precip_level
pred_df$year <- newdat$year


pred_df <- pred_df %>%
  mutate(
    Temp_level = factor(Temp_level, levels = c("6.5", "8.5", "10.5")),
    climate_group = paste(Temp_level, Precip_level, sep = "_")
  )

# Plot it
ggplot(pred_df, aes(x = RDA1, y = RDA2, colour = Precip_level)) +
  geom_point(data = site_scores, aes(x = RDA1, y = RDA2),
             colour = "grey30", alpha = 0.2, size = 1) +  # background points
  geom_path(aes(group = climate_group, color = "black"), linewidth = 1) +
  geom_point(data = pred_df %>% filter(year == 2009),
             aes(shape = Precip_level, color = Precip_level),
             size = 3,
             show.legend = FALSE) +
  scale_color_manual(name = "Annual precipitation", values = c(Precip_palette, "black")) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
    
  ) +
  labs(
    x = "RDA1",
    y = "RDA2",
    color = "Summer temperature",
    shape = "Summer temperature"
  ) +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  facet_wrap(~Temp_level)



#### Mixed effect model plots ####

## New after the model selection process:

# Plot climate predictions
temp_traits_dat <- predictions_climate |> 
  filter(Trait_trans %in% temp_traits) |> 
  filter(Trait_trans != "Wet_Mass_g_log")

precip_traits_dat <- predictions_climate |> 
  filter(Trait_trans %in% precip_traits)

interactions_traits_dat <- predictions_climate |> 
  filter(Trait_trans %in% interaction_traits)


pretty_trait_names_LES <- c(
  "N_percent" = "Leaf N (%)",
  "CN_ratio" = "Leaf C/N",
  "LDMC" = "LDMC",
  "Leaf_Thickness_Ave_mm" = "Leaf Thickness"
)


pretty_trait_names_size <- c(
  "C_percent" = "Leaf C (%)",
  "Leaf_Area_cm2_log" = "Leaf Area",
  "Plant_Height_mm_log" = "Plant Height",
  "Dry_Mass_g_log" = "Leaf Dry Mass"
)



plot_temp_sizetraits <- temp_traits_dat |> 
  mutate(significant = case_when(Trait_trans %in% c("Dry_Mass_g_log", "Leaf_Area_cm2_log", "Plant_Height_mm_log") ~ "YES",
                                 Trait_trans == "C_percent" ~ "Nearly")) |>
  mutate(Trait_pretty = recode(Trait_trans, !!!pretty_trait_names)) |> 
  ggplot(aes(x = Temp_decade, y = predicted)) +
  geom_point(aes(x = Temp_decade, y = value, color = "grey"), 
             data = (models_pred |> 
                       select(Trait_trans, Trait_pretty, data) |> 
                       unnest(data) |> 
                       filter(Trait_trans %in% temp_traits) |> 
                       filter(Trait_trans != "Wet_Mass_g_log"))) +
  geom_ribbon(aes(ymin = predicted - se_predicted, ymax = predicted + se_predicted, fill = significant), alpha = 0.3) +
  geom_line(aes(color = significant, linetype = significant), size = 1) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~Trait_pretty, scales = "free_y", ncol = 1) +
  theme_minimal(base_size = 15) +
  scale_color_manual(values = c("#dd7631", "#dd7631", "grey")) +
  scale_fill_manual(values = c("#dd7631", "#dd7631")) +
  scale_linetype_manual(values = c(5,1)) +
  theme(legend.position = "none",
        axis.ticks.x = element_line()) +
  scale_x_continuous()
  
plot_precip_LEtraits <- precip_traits_dat |> 
  mutate(significant = case_when(Trait_trans %in% c("CN_ratio", "LDMC", "Leaf_Thickness_Ave_mm") ~ "NO",
                                 Trait_trans == "N_percent" ~ "Nearly")) |> 
  mutate(Trait_pretty = recode(Trait_trans, !!!pretty_trait_names)) |> 
  ggplot( aes(x = Precip_decade, y = predicted)) +
    geom_point(aes(x = Precip_decade, y = value, color = "grey"), 
               data = (models_pred |> 
                         select(Trait_trans, Trait_pretty, data) |> 
                         unnest(data) |> 
                         filter(Trait_trans %in% precip_traits))) +
  geom_ribbon(aes(ymin = predicted - se_predicted, ymax = predicted + se_predicted, fill = significant), alpha = 0.3) +
  geom_line(aes(color = significant, linetype = significant), size = 1) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~Trait_pretty, scales = "free_y", ncol = 1) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none",
        axis.ticks.x = element_line()) +
  scale_color_manual(values = c("black", "#2E75B6", "grey")) +
  scale_fill_manual(values = c("black", "#2E75B6")) +
  scale_linetype_manual(values = c(3, 5)) +
  scale_x_continuous()


plot_interaction_SLA <- 
ggplot(interactions_traits_dat, aes(x = Precip_decade, y = predicted)) +
   geom_point(aes(x = Precip_decade, y = value, color = Temp_level), 
              data = (models_pred |> 
                        select(Trait_trans, data) |> 
                        unnest(data) |> 
                        filter(Trait_trans %in% interaction_traits) |> 
                        select(Trait_trans, Precip_decade, value, Temp_level)
                      )) +
  geom_line(aes(color = as.factor(Temp_decade)), size = 1) +
  geom_ribbon(aes(ymin = predicted - se_predicted, ymax = predicted + se_predicted, fill = as.factor(Temp_decade)), alpha = 0.5) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c("#d8c593", "#dd7631", "#bb3b0e")) + 
  scale_color_manual(values = c("#d8c593", "#dd7631", "#bb3b0e")) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none",
        axis.ticks.x = element_line()) +
  scale_x_continuous()


Leaf_economic_layout <- ggarrange(plot_precip_LEtraits, plot_interaction_SLA, nrow = 2, ncol = 1,
                                  heights = c(4, 1))

Size_layout <- ggarrange(plot_temp_sizetraits, plot_spacer(), nrow = 2, ncol = 1,
                         heights = c(4,1))

combined_plots_spatial <- ggarrange(Size_layout, Leaf_economic_layout, ncol = 2)


#ggsave(plot = combined_plots_spatial, filename = "Linear_mixed_effect_models_spatial.pdf",  width = 17, height = 26, units = "cm", dpi = 300)


# Plot temporal predictions
# ggplot(predictions_temporal, aes(x = year, y = predicted_year)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = predicted_year - se_predicted_year, ymax = predicted_year + se_predicted_year), alpha = 0.2) +
#   labs(title = "Temporal Model Predictions", x = "Year", y = "Predicted Value") +
#   facet_wrap(~Trait_trans)

temp_traits_dat_temporal <- predictions_temporal |> 
  filter(Trait_trans %in% temp_traits) |> 
  filter(Trait_trans != "Wet_Mass_g_log")

precip_traits_dat_temporal <- predictions_temporal |> 
  filter(Trait_trans %in% precip_traits)

interactions_traits_dat_temporal <- predictions_temporal |> 
  filter(Trait_trans %in% interaction_traits)



plot_year_sizetraits <- predictions_temporal |> 
  filter(Trait_trans %in% temp_traits) |> 
  filter(Trait_trans != "Wet_Mass_g_log") |> 
  mutate(significant = case_when(Trait_trans %in% c("Dry_Mass_g_log", "Leaf_Area_cm2_log") ~ "YES",
                                 Trait_trans == "Plant_Height_mm_log" ~ "Nearly",
                                 Trait_trans == "C_percent" ~ "No"),
         Trait_pretty = pretty_trait_names_size[Trait_trans]
         ) |> 
ggplot(aes(x = year, y = predicted_year)) +
  geom_point(aes(x = year, y = value, color = "grey"), 
             data = (models_pred |> 
                       select(Trait_trans, Trait_pretty, data) |> 
                       unnest(data) |> 
                       filter(Trait_trans %in% temp_traits) |> 
                       filter(Trait_trans != "Wet_Mass_g_log"))) +
  geom_ribbon(aes(ymin = predicted_year - se_predicted_year, ymax = predicted_year + se_predicted_year, fill = as.factor(Temp_decade)), alpha = 0.2) +
  geom_line(aes(color = interaction(significant, Temp_decade), linetype = significant), size = 1) +
  labs(title = NULL, x = NULL, y = NULL) +
  facet_wrap(~Trait_pretty, scales = "free_y", ncol = 1) +
  scale_x_continuous(
    breaks = c(2009, 2011, 2013, 2015, 2017, 2019), 
    labels = c(2009, 2011, 2013, 2015, 2017, 2019)
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_line(), 
    legend.position = "none"
  ) +
  scale_color_manual(values = c("YES.6.5" = Temp_palette[1], "YES.8.5" = Temp_palette[2], "YES.10.5" = Temp_palette[3],
                                "Nearly.6.5" = Temp_palette[1], "Nearly.8.5" = Temp_palette[2], "Nearly.10.5" = Temp_palette[3],
                                "No.6.5" = "black", "No.8.5" = "black", "No.10.5" = "black")) +
  scale_fill_manual(values = Temp_palette) +
  scale_linetype_manual(values = c("YES" = 1, "Nearly" = 5, "No" = 3)) 


plot_year_LEStraits <- predictions_temporal |> 
  filter(Trait_trans %in% precip_traits) |> 
  mutate(significant = case_when(Trait_trans %in% c("CN_ratio", "N_percent") ~ "No",
                                 Trait_trans == "LDMC" ~ "YES",
                                 Trait_trans == "Leaf_Thickness_Ave_mm"  ~ "Nearly"),
         Trait_pretty = pretty_trait_names_LES[Trait_trans]
         ) |> 
  ggplot(aes(x = year, y = predicted_year)) +
  geom_point(aes(x = year, y = value, color = "grey"), 
             data = (models_pred |> 
                       select(Trait_trans, Trait_pretty, data) |> 
                       unnest(data) |> 
                       filter(Trait_trans %in% precip_traits))) +
  geom_ribbon(aes(ymin = predicted_year - se_predicted_year, ymax = predicted_year + se_predicted_year, fill = as.factor(Precip_decade)), alpha = 0.2) +
  geom_line(aes(color = interaction(significant, Precip_decade), linetype = significant), size = 1) +
  labs(title = NULL, x = NULL, y = NULL) +
  facet_wrap(~Trait_pretty, scales = "free_y", ncol = 1) +
  scale_x_continuous(
    breaks = c(2009, 2011, 2013, 2015, 2017, 2019), 
    labels = c(2009, 2011, 2013, 2015, 2017, 2019)
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_line(), 
    legend.position = "none"
  ) +
  scale_color_manual(values = c("YES.1" = Precip_palette[1], "YES.1.45" = Precip_palette[2], "YES.2.4" = Precip_palette[3], "YES.3.5" = Precip_palette[4],
                                "Nearly.1" = Precip_palette[1], "Nearly.1.45" = Precip_palette[2], "Nearly.2.4" = Precip_palette[3], "Nearly.3.5" = Precip_palette[4],
                                "No.1" = "black", "No.1.45" = "black", "No.2.4" = "black", "No.3.5" = "black")) +
  scale_fill_manual(values = Precip_palette) +
  scale_linetype_manual(values = c("YES" = 1, "Nearly" = 5, "No" = 3)) +
  theme(legend.position = "none")


Precip_decade_factor <- c(1.0, 1.45, 2.4, 3.5)

plot_year_SLA <- predictions_temporal |> 
  filter(Trait_trans %in% interaction_traits) |> 
  ggplot(aes(x = year, y = predicted_year)) +
  geom_point(aes(x = year, y = value, color = as.factor(Temp_decade)), 
             data = (models_pred |> 
                       select(Trait_trans, data) |> 
                       unnest(data) |> 
                       filter(Trait_trans %in% interaction_traits) |> 
                       mutate(Precip_decade = as.factor(recode(siteID, Ulvehaugen = 1.0, Alrust = 1.0, Fauske = 1.0, Lavisdalen = 1.45, Hogsete = 1.45, Vikesland = 1.45, Gudmedalen = 2.4, Rambera = 2.4, Arhelleren = 2.4, Skjelingahaugen = 3.5, Veskre = 3.5, Ovstedalen = 3.5))) |> 
                       select(Trait_trans, year, value, Temp_level, Precip_decade) |> 
                       rename(Temp_decade = Temp_level))) +
  geom_line(aes(color = as.factor(Temp_decade)), size = 1, linetype = 3) +
  geom_ribbon(aes(ymin = predicted_year - se_predicted_year, ymax = predicted_year + se_predicted_year, fill = as.factor(Temp_decade)), alpha = 0.2) +
  labs(x = NULL, y = NULL, title = "SLA") +
  scale_fill_manual(values = Temp_palette) + 
  scale_color_manual(values = Temp_palette) +
  scale_x_continuous(
    breaks = c(2009, 2011, 2013, 2015, 2017, 2019), 
    labels = c(2009, 2011, 2013, 2015, 2017, 2019)
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_line(), 
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank()
  ) +
  facet_wrap(~Precip_decade, nrow = 1) 



combined_columns <- ggarrange(plot_year_sizetraits, plot_year_LEStraits,ncol = 2, heights = c(4, 4))

combined_plots_temporal <- ggarrange(combined_columns, plot_year_SLA, nrow = 2, heights = c(6, 2))

#ggsave(plot = combined_plots_temporal, filename = "Linear_mixed_effect_models_temporal.pdf",  width = 18, height = 28, units = "cm", dpi = 300)



#### Comparing spatial and temporal patterns ####

custom_colors <- c(
  "Temperature Temporal with ITV" = Temp_palette[1],
  "Temperature Spatial with ITV" = Temp_palette[3],
  "Temperature Spatial wo ITV" = Temp_palette[2],
  "Temperature Temporal wo ITV" = "grey", 
  "Precipitation Temporal with ITV" = Precip_palette[2],
  "Precipitation Spatial with ITV" = Precip_palette[4],
  "Precipitation Spatial wo ITV" = Precip_palette[3],
  "Precipitation Temporal wo ITV" = Precip_palette[1]
)

custom_colors2 <- c(
  "Temperature Temporal with ITV" = Temp_palette[1],
  "Temperature Spatial with ITV" = Temp_palette[3],
  "Temperature Spatial without ITV" = Temp_palette[2],
  "Temperature Temporal without ITV" = "grey", 
  "Precipitation Temporal with ITV" = Precip_palette[2],
  "Precipitation Spatial with ITV" = Precip_palette[4],
  "Precipitation Spatial without ITV" = Precip_palette[3],
  "Precipitation Temporal without ITV" = Precip_palette[1]
)


trait_order <- rev(c(temp_traits, interaction_traits, precip_traits))

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


###TEST


# Step 1: Wrangle plotting data
SpatialTemporal_comparison_all <- SpatialTemporal_comparison_and_ITV |> 
  mutate(
    trend_short = case_when(
      str_detect(trend2, "spatial") ~ "Spatial",
      str_detect(trend2, "temporal") ~ "Temporal"
    ),
    ITV_label = case_when(
      str_detect(trend2, "withoutITV") ~ "wo ITV",
      str_detect(trend2, "withITV") ~ "with ITV"
    ),
    driver = case_when(
      Trait_trans %in% temp_traits ~ "Temperature",
      Trait_trans %in% precip_traits ~ "Precipitation",
      Trait_trans %in% interaction_traits ~ "Temperature"  # Assumes interaction traits relate to temp
    ),
    driver_trend = paste(driver, trend_short, ITV_label),
    pattern_type = ifelse(significant == "YES", "Significant", "Non-Significant"),
    significance_alpha = if_else(significant == "YES", 1, 0.4)
  ) |> 
  mutate(
    driver_trend = factor(driver_trend, levels = c(
      "Temperature Temporal wo ITV", "Temperature Temporal with ITV", 
       "Temperature Spatial wo ITV", "Temperature Spatial with ITV",
       "Precipitation Temporal wo ITV", "Precipitation Temporal with ITV",
       "Precipitation Spatial wo ITV", "Precipitation Spatial with ITV"
    )),
    Trait_trans = factor(Trait_trans, levels = trait_order),
    Trait_pretty = recode(Trait_trans, !!!pretty_trait_names)
  ) |> 
  filter(!Trait_trans %in% c("SLA_cm2_g", "Wet_Mass_g_log")) |> 
  filter(!(trend_short == "Temporal" & ITV_label == "wo ITV"))

# Step 2: Make plot
compare_spatial_temporal_plot_all <- ggplot(SpatialTemporal_comparison_all, 
                                            aes(x = estimate, y = Trait_pretty, 
                                                fill = driver_trend, 
                                                #pattern = pattern_type,
                                                group = driver_trend)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.55,
           color = "black") +
  # geom_col_pattern(
  #   position = position_dodge(width = 0.7),
  #   width = 0.55,
  #   color = "black",
  #   linewidth = 0.3,
  #   pattern_density = 0.25,
  #   pattern_spacing = 0.015,
  #   pattern_fill = "black",
  #   pattern_alpha = 0.6
  # ) +
  scale_fill_manual(values = custom_colors) +
  scale_pattern_manual(values = c("Significant" = "none", "Non-Significant" = "stripe")) +
  # labs(
  #   x = "Effect Size (per °C temperature or m precipitation)",
  #   y = "Trait",
  #   fill = "Driver & Trend",
  #   pattern = "Significance",
  #   title = ""
  # ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank()
  ) +
  geom_vline(xintercept = 0, color = "black", size = 0.7, linetype = "solid")


#ggsave(plot = compare_spatial_temporal_plot_all, filename = "Compare_spatial_temporal_figure.pdf",  width = 25, height = 14, units = "cm", dpi = 300)

### SLA intercation ###

#Order
common_driver_trend_levels <- c("Temperature Temporal without ITV", "Temperature Temporal with ITV", 
                                "Temperature Spatial without ITV", "Temperature Spatial with ITV",
                                "Precipitation Temporal without ITV", "Precipitation Temporal with ITV",
                                "Precipitation Spatial without ITV", "Precipitation Spatial with ITV")
# Set order for x-axis (which will be y-axis after flipping)
level_order <- c(
  "Precipitation (temporal)",
  paste0("Precipitation trend at: ", temp_levels, " °C"),
  "Temperature (temporal)",
  paste0("Temperature trend at: ", precip_levels, " m/year")
)


effects_df2 <- effects_df_full |> 
  filter(!(source == "Temporal" & data == "without ITV")) |> 
  mutate(driver_trend = paste(variable, source, data, sep = " "),
         pattern_type = ifelse(significant == "NO", "Non-Significant", "Significant")) |> 
  mutate(driver_trend = factor(driver_trend, levels = common_driver_trend_levels)) |> 
  mutate(level = factor(level, levels = level_order))




# Plot
SLA_plot <- ggplot(effects_df2, 
                   aes(x = effect, y = level, 
                       fill = driver_trend, 
                       #pattern = pattern_type,
                       group = driver_trend)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.55,
           color = "black") +
  # geom_col_pattern(
  #   position = position_dodge(width = 0.6),
  #   width = 0.5,
  #   color = "black",
  #   size = 0.3,
  #   pattern_density = 0.25,
  #   pattern_spacing = 0.015,
  #   pattern_fill = "black",
  #   pattern_alpha = 0.6
  # ) +
  scale_fill_manual(values = custom_colors2) +
  scale_pattern_manual(values = c("Significant" = "none", "Non-Significant" = "stripe")) +
  labs(
    x = "Effect Size (per °C temperature or m precipitation)",
    y = "",
    fill = "Driver & Trend",
    pattern = "Significance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  ) +
  geom_vline(xintercept = 0, color = "black", size = 0.7, linetype = "solid")

SLA_plot


combined_plot <- compare_spatial_temporal_plot_all  / SLA_plot + 
  plot_layout(heights = c(2.5, 1.5)) #& 
  #theme(legend.position = "right")

combined_plot

#ggsave(plot = combined_plot, filename = "SLA_Compare_spatial_temporal_figure.pdf",  width = 25, height = 22, units = "cm", dpi = 300)


################################################################################################
#####################                                                      #####################
#####################                        OLD CODE                      #####################
#####################                                                      #####################
################################################################################################



##NEW
#augment instead of predict (a wrapper broom.mixed)

#augment(model) |> 
 #ggplot()
  
models |> 
  ungroup() |> 
  filter(Trait_trans == "SLA_cm2_g") |> 
  select(data) |> 
  unnest(cols = c(data))


plot_space_and_time <-function(dat, trait) {
  
  dat2 <- dat |> 
    filter(Trait_trans == trait) |> 
    unnest(data) |> 
    ungroup() |> 
    mutate(lower = predicted_year - se_predicted_year,
           upper = predicted_year + se_predicted_year)
  
  plot <- ggplot(dat2, aes(x = year, y = value)) +
    geom_point(aes(color = Precip_level)) +
    geom_line(aes(x = year, y = predicted_year, color = Precip_level), linewidth = 1) +
    geom_ribbon(aes(x = year, y = predicted_year, ymin = lower, ymax = upper, fill = Precip_level), data = dat2, alpha = 0.22) +
    facet_wrap(~ Temp_level) +
    labs(x = "Year", y = trait, color = "Precipitation Level") +
    scale_color_manual(values = Precip_palette) +
    scale_fill_manual(values = Precip_palette) +
    theme_minimal(base_size = 12) +
    guides(fill = "none")
    
    return(plot)
}

#Only plotting Plant_Height_mm_log now as it is the only significant one with time.
#Other traits are called: "CN_ratio", "C_percent", "Dry_Mass_g_log", "LDMC", 
# "Leaf_Area_cm2_log", "Leaf_Thickness_Ave_mm", "N_percent", "Plant_Height_mm_log", "SLA_cm2_g", "Wet_Mass_g_log"   
plot_space_time_Height <- plot_space_and_time(models, "Plant_Height_mm_log")
plot_space_time_LeafThickness <- plot_space_and_time(models, "Leaf_Thickness_Ave_mm")

#ggsave(plot = plot_space_time_Height, filename = "Height_in_space_and_time.pdf",  width = 20, height = 10, units = "cm")
#ggsave(plot = plot_space_time_LeafThickness, filename = "LeafThickness_in_space_and_time.pdf",  width = 20, height = 10, units = "cm")


#### Without year

plot_space <-function(dat, trait) {

  
  dat1 <- dat |> 
    filter(Trait_trans == trait) |> 
    unnest(data) |> 
    ungroup() |> 
    mutate(Precip_decade_vec = case_when(Precip_level == 600 ~ 1.0,
                                         Precip_level == 1200 ~ 1.45,
                                         Precip_level == 2000 ~ 2.4,
                                         Precip_level == 2700 ~ 3.5,
                                         TRUE ~ NA_real_))
  
  dat2 <- dat |> 
    filter(Trait_trans == trait) |> 
    unnest(predicted_newdata) |> 
    ungroup() |> 
    mutate(lower = predicted - se_predicted,
           upper = predicted + se_predicted)
  
  plot <- ggplot(dat1, aes(x = Temp_decade, y = value)) +
    geom_point(aes(color = factor(Precip_decade_vec))) +
    geom_line(aes(x = Temp_decade, y = predicted, color = factor(Precip_decade)), data = dat2, linewidth = 1) +
    geom_ribbon(aes(x = Temp_decade, y = predicted, ymin = lower, ymax = upper, fill = factor(Precip_decade)), data = dat2, alpha = 0.25) +
    labs(x = "Summer temperature", y = trait, color = "Precipitation Level") +
    scale_color_manual(values = Precip_palette) +
    scale_fill_manual(values = Precip_palette) +
    theme_minimal(base_size = 15) +
    guides(fill = "none")
  
  return(plot)
}

#### Plotting all the spatial models ----

plot_space_SLA <- plot_space(models, "SLA_cm2_g")
plot_space_LDMC <- plot_space(models, "LDMC")
plot_space_LeafThickness <- plot_space(models, "Leaf_Thickness_Ave_mm")

plot_space_CN <- plot_space(models, "CN_ratio")
plot_space_N <- plot_space(models, "N_percent")
plot_space_Drymass <- plot_space(models, "Dry_Mass_g_log")
plot_space_LeafArea <- plot_space(models, "Leaf_Area_cm2_log")
plot_space_C <- plot_space(models, "C_percent")
plot_space_Height <- plot_space(models, "Plant_Height_mm_log")

figure <- ggarrange(plot_space_LeafArea, plot_space_SLA, 
                    plot_space_Height, plot_space_LDMC, 
                    plot_space_Drymass, plot_space_LeafThickness,  
                    plot_space_C, plot_space_N,  
                    plot_space_CN,   
                    nrow = 5, ncol = 2, common.legend = TRUE, legend = "bottom")


#ggsave(plot = figure, filename = "Traits_in_space.pdf",  width = 14, height = 28, units = "cm")
#ggsave(plot = figure, filename = "Traits_in_space.jpg",  width = 14, height = 28, units = "cm")


##OLD
plot_predictions_space <-function(dat, trait, moment, newdata) {
  
  dat2 <- dat %>%
    filter(Trait_trans == trait,
           moments == moment,
           n == 75) %>% 
    unnest(data) %>% 
    ungroup()
 
    
    plot <- ggplot(dat2, aes(x = Temp_decade, y = value)) +
    geom_jitter(color = "lightgrey") +
    geom_line(aes(x = Temp_decade, y = predicted, color = factor(Precip_decade)), data=newdata, size = 1, show.legend = TRUE) +
    scale_color_manual(values = Precip_palette) +
    theme_minimal(base_size = 15) 
  
  return(plot)
}

plot_predictions_space_precip <-function(dat, trait, moment, model) {
  
  dat2 <- dat %>%
    filter(Trait_trans == trait,
           moments == moment,
           n == 75) %>% 
    unnest(data) %>% 
    ungroup()
  
  newdata <- expand.grid(Temp_decade = c(6.5, 9, 11), 
                         Precip_decade = seq(0.5,4.5, length = 200), 
                         siteID = c("Alrust", "Arhelleren", "Fauske", "Gudmedalen", "Hogsete", "Lavisdalen", "Ovstedalen", "Rambera", "Skjelingahaugen", "Ulvehaugen", "Veskre", "Vikesland"),
                         Precip_annomalies = 0,
                         Temp_annomalies = 0)
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE)
  
  
  plot <- ggplot() +
    geom_jitter(aes (x = Precip_decade, y = value), data = dat2, color = "lightgrey") +
    geom_line(aes(x = Precip_decade, y = predicted, color = factor(Temp_decade)), data = newdata, size = 1, show.legend = TRUE) +
    scale_color_manual(values = c("#DDA131","#C46B00", "#bb3b0e")) +
    theme_minimal(base_size = 15) +
    theme(legend.position = "bottom") 
  
  return(plot)
}

#Find the average Temp anomalies for each site to use in the model predictions
env %>%  dplyr::select(siteID, Temp_annomalies) %>%
  mutate(Temp_annomalies = abs(Temp_annomalies)) %>% 
  group_by(siteID) %>% 
  mutate(Temp = mean(Temp_annomalies)) %>%
  dplyr::select(-Temp_annomalies) %>%
  unique()

#Find the average precip anomalies for each site to use in the model predictions
env %>%  dplyr::select(siteID, Precip_annomalies) %>%
  mutate(Precip_annomalies = abs(Precip_annomalies)) %>% 
  group_by(siteID) %>% 
  mutate(Precip = mean(Precip_annomalies)) %>%
  dplyr::select(-Precip_annomalies) %>%
  unique()

plot_predictions_time <-function(dat, trait, moment, precip_level, model, clim) {
  
  dat2 <- dat %>%
    filter(Trait_trans == trait,
           moments == moment,
           n == 75) %>% 
    unnest(data) %>% 
    ungroup()
  
  newdata_alpine <- crossing(Precip_decade = case_when(precip_level == "Wettest" ~ 3.5,
                                                     precip_level == "Wet" ~ 2.4,
                                                     precip_level == "Dry" ~ 1.45,
                                                     precip_level == "Driest" ~ 1.0),
                           Temp_decade = 6.5,
                           siteID = case_when(precip_level == "Wettest" ~ "Skjelingahaugen",
                                              precip_level == "Wet" ~ "Gudmedalen",
                                              precip_level == "Dry" ~ "Lavisdalen",
                                              precip_level == "Driest" ~ "Ulvehaugen"),
                           Precip_annomalies = case_when({{clim}} == "Precip_annomalies" ~ seq(-2, 3, length = 200)),
                           Temp_annomalies = case_when({{clim}} == "Precip_annomalies" & precip_level == "Wettest" ~ 0.911,
                                                       {{clim}} == "Precip_annomalies" & precip_level == "Wet" ~ 1.03,
                                                       {{clim}} == "Precip_annomalies" & precip_level == "Dry" ~ 1.03,
                                                       {{clim}} == "Precip_annomalies" & precip_level == "Driest" ~ 0.985,
                                                       {{clim}} == "Temp_annomalies" ~ seq(-3, 2, length = 200)))
  
  newdata_subalpine <- crossing(Precip_decade = case_when(precip_level == "Wettest" ~ 3.5,
                                                     precip_level == "Wet" ~ 2.4,
                                                     precip_level == "Dry" ~ 1.45,
                                                     precip_level == "Driest" ~ 1.0),
                           Temp_decade = 9.0,
                           siteID = case_when(precip_level == "Wettest" ~ "Veskre",
                                              precip_level == "Wet" ~ "Rambera",
                                              precip_level == "Dry" ~ "Hogsete",
                                              precip_level == "Driest" ~ "Alrust"),
                           Precip_annomalies = case_when({{clim}} == "Precip_annomalies" ~ seq(-2, 3, length = 200)),
                           Temp_annomalies = case_when({{clim}} == "Precip_annomalies" & precip_level == "Wettest" ~ 0.902,
                                                       {{clim}} == "Precip_annomalies" & precip_level == "Wet" ~ 0.855,
                                                       {{clim}} == "Precip_annomalies" & precip_level == "Dry" ~ 0.999,
                                                       {{clim}} == "Precip_annomalies" & precip_level == "Driest" ~ 0.887,
                                                       {{clim}} == "Temp_annomalies" ~ seq(-3, 2, length = 200)))
  
  newdata_boreal <- crossing(Precip_decade = case_when(precip_level == "Wettest" ~ 3.5,
                                                     precip_level == "Wet" ~ 2.4,
                                                     precip_level == "Dry" ~ 1.45,
                                                     precip_level == "Driest" ~ 1.0),
                           Temp_decade = 11,
                           siteID = case_when(precip_level == "Wettest" ~ "Ovstedalen",
                                              precip_level == "Wet" ~ "Arhelleren",
                                              precip_level == "Dry" ~ "Vikesland",
                                              precip_level == "Driest" ~ "Fauske"),
                           Precip_annomalies = case_when({{clim}} == "Precip_annomalies" ~ seq(-2, 3, length = 200)),
                           Temp_annomalies = case_when({{clim}} == "Precip_annomalies" & precip_level == "Wettest" ~ 0.892,
                                                       {{clim}} == "Precip_annomalies" & precip_level == "Wet" ~ 0.990,
                                                       {{clim}} == "Precip_annomalies" & precip_level == "Dry" ~ 1.01,
                                                       {{clim}} == "Precip_annomalies" & precip_level == "Driest" ~ 1.18,
                                                       {{clim}} == "Temp_annomalies" ~ seq(-3, 2, length = 200)))
  
  
  newdata_alpine$predicted <- predict(object = model, newdata = newdata_alpine, re.form = NA, allow.new.levels=TRUE)
  newdata_subalpine$predicted <- predict(object = model, newdata = newdata_subalpine, re.form = NA, allow.new.levels=TRUE)
  newdata_boreal$predicted <- predict(object = model, newdata = newdata_boreal, re.form = NA, allow.new.levels=TRUE)
  
  highlighted <- dat2 %>% 
    filter(siteID == case_when(precip_level == "Wettest" ~ c("Skjelingahaugen", "Veskre", "Ovstedalen"),
                               precip_level == "Wet" ~ c( "Gudmedalen", "Rambera", "Arhelleren"),
                               precip_level == "Dry" ~ c("Lavisdalen", "Hogsete", "Vikesland"),
                               precip_level == "Driest" ~ c("Ulvehaugen", "Alrust", "Fauske"))) 
  
  plot <- ggplot(aes(x = .data[[clim]], y = value), data = dat2) +
    geom_point(color = "grey95") +
    geom_point(data = highlighted, aes(color = Temp_level)) +
    geom_line(aes(x = .data[[clim]], y = predicted), data = newdata_boreal, size = 1, show.legend = TRUE, color = "#bb3b0e") +
    geom_line(aes(x = .data[[clim]], y = predicted), data = newdata_subalpine, size = 1, show.legend = TRUE, color = "#dd7631") +
    geom_line(aes(x = .data[[clim]], y = predicted), data = newdata_alpine, size = 1, show.legend = TRUE, color = "#d8c593") +
    theme_minimal(base_size = 15) + 
    xlab(ifelse({{clim}} == "Precip_annomalies", "Precipitation annomalies (m)","Temperature annomalies (C)")) +
    xlim(-2, 3) +
    ylim(range(dat2$value)) +
    theme(axis.title.y = element_blank(), axis.title.x = element_blank(), legend.position = "none") +
    scale_color_manual(values = c("#d8c593", "#dd7631", "#bb3b0e"))
  
  return(plot)
}


plot_predictions_time_temp <-function(dat, trait, moment, temp_level, model) {
  
  dat2 <- dat %>%
    filter(Trait_trans == trait,
           moments == moment,
           n == 75) %>% 
    unnest(data) %>% 
    ungroup()
  
  newdata_wettest <- crossing(Temp_decade = case_when(temp_level == "Boreal" ~ 11.0,
                                                       temp_level == "Sub_alpine" ~ 9.0,
                                                       temp_level == "Alpine" ~ 6.5),
                             Precip_decade = 3.5,
                             siteID = case_when(temp_level == "Boreal" ~ "Ovstedal",
                                                temp_level == "Sub_alpine" ~ "Veskre",
                                                temp_level == "Alpine" ~ "Skjelingagaugen"),
                             Temp_annomalies = seq(-2, 1.2, length = 200),
                             Precip_annomalies = case_when(temp_level == "Boreal" ~ 1.71,
                                                           temp_level == "Sub_alpine" ~ 0.597,
                                                           temp_level == "Alpine" ~ 0.910))
  
  newdata_wet <- crossing(Temp_decade = case_when(temp_level == "Boreal" ~ 11.0,
                                                 temp_level == "Sub_alpine" ~ 9.0,
                                                 temp_level == "Alpine" ~ 6.5),
                         Precip_decade = 2.4,
                         siteID = case_when(temp_level == "Boreal" ~ "Arhelleren",
                                            temp_level == "Sub_alpine" ~ "Rambera",
                                            temp_level == "Alpine" ~ "Gudmedalen"),
                         Temp_annomalies = seq(-2, 1.2, length = 200),
                         Precip_annomalies = case_when(temp_level == "Boreal" ~ 1.35,
                                                       temp_level == "Sub_alpine" ~ 0.320,
                                                       temp_level == "Alpine" ~ 0.510))
  
  
  newdata_dry <- crossing(Temp_decade = case_when(temp_level == "Boreal" ~ 11.0,
                                                 temp_level == "Sub_alpine" ~ 9.0,
                                                 temp_level == "Alpine" ~ 6.5),
                         Precip_decade = 1.45,
                         siteID = case_when(temp_level == "Boreal" ~ "Vikesland",
                                            temp_level == "Sub_alpine" ~ "Hogsete",
                                            temp_level == "Alpine" ~ "Lavisdalen"),
                         Temp_annomalies = seq(-2, 1.2, length = 200),
                         Precip_annomalies = case_when(temp_level == "Boreal" ~ 0.307,
                                                       temp_level == "Sub_alpine" ~ 0.318,
                                                       temp_level == "Alpine" ~ 0.412))
  
  
  newdata_driest <- crossing(Temp_decade = case_when(temp_level == "Boreal" ~ 11.0,
                                                  temp_level == "Sub_alpine" ~ 9.0,
                                                  temp_level == "Alpine" ~ 6.5),
                          Precip_decade = 1.0,
                          siteID = case_when(temp_level == "Boreal" ~ "Fauske",
                                             temp_level == "Sub_alpine" ~ "Alrust",
                                             temp_level == "Alpine" ~ "Ulvehaugen"),
                          Temp_annomalies = seq(-2, 1.2, length = 200),
                          Precip_annomalies = case_when(temp_level == "Boreal" ~ 0.246,
                                                        temp_level == "Sub_alpine" ~ 0.446,
                                                        temp_level == "Alpine" ~ 0.629))
  
  
  
  newdata_wettest$predicted <- predict(object = model, newdata = newdata_wettest, re.form = NA, allow.new.levels=TRUE)
  newdata_wet$predicted <- predict(object = model, newdata = newdata_wet, re.form = NA, allow.new.levels=TRUE)
  newdata_dry$predicted <- predict(object = model, newdata = newdata_dry, re.form = NA, allow.new.levels=TRUE)
  newdata_driest$predicted <- predict(object = model, newdata = newdata_driest, re.form = NA, allow.new.levels=TRUE)
  
  highlighted <- dat2 %>% 
    filter(siteID %in% case_when(temp_level == "Boreal" ~ c("Ovstedalen","Arhelleren", "Vikesland", "Fauske"),
                               temp_level == "Sub_alpine" ~ c("Rambera", "Veskre", "Alrust", "Hogsete"),
                               temp_level == "Alpine" ~ c("Lavisdalen", "Gudmedalen", "Skjelingahaugen", "Ulvehaugen"))) 
  
  plot <- ggplot(aes(x = Temp_annomalies, y = value), data = dat2) +
    geom_point(color = "grey95") +
    geom_point(data = highlighted, aes(color = Precip_level)) +
    geom_line(aes(x = Temp_annomalies, y = predicted), data = newdata_wettest, size = 1, show.legend = TRUE, color = "#213964") +
    geom_line(aes(x = Temp_annomalies, y = predicted), data = newdata_wet, size = 1, show.legend = TRUE, color = "#2E75B6") +
    geom_line(aes(x = Temp_annomalies, y = predicted), data = newdata_dry, size = 1, show.legend = TRUE, color = "#89B7E1") +
    geom_line(aes(x = Temp_annomalies, y = predicted), data = newdata_driest, size = 1, show.legend = TRUE, color = "#BAD8F7") +
    theme_minimal(base_size = 15) + 
    xlab("Temperature annomalies (C)") +
    xlim(-2, 1.2) +
    ylim(range(dat2$value)) +
    theme(axis.title.y = element_blank(), axis.title.x = element_blank(), legend.position = "none") +
    scale_color_manual(values = c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964"))
  
  return(plot)
}


Precip_palette <- c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")

## Make plot for LDMC changes in temporal climate ad different sites in the climate grid

LDMC_Driest <- plot_predictions_time(memodel_data_fullcommunity_nottransformed, "LDMC", "mean", "Driest", LDMC_mean_sum_yc,"Precip_annomalies") +
  labs(y = "",  title = "Driest") +
  theme(plot.title = element_text(hjust = 0.5))

LDMC_Dry <- plot_predictions_time(memodel_data_fullcommunity_nottransformed, "LDMC", "mean", "Dry", LDMC_mean_sum_yc,"Precip_annomalies") +
  labs(y = "",  title = "Dry") +
  theme(plot.title = element_text(hjust = 0.5))

LDMC_Wet <- plot_predictions_time(memodel_data_fullcommunity_nottransformed, "LDMC", "mean", "Wet", LDMC_mean_sum_yc, "Precip_annomalies") +
  labs(y = "",  title = "Wet") +
  theme(plot.title = element_text(hjust = 0.5))

LDMC_Wettest <- plot_predictions_time(memodel_data_fullcommunity_nottransformed, "LDMC", "mean", "Wettest", LDMC_mean_sum_yc, "Precip_annomalies") +
  labs(y = "",  title = "Wettest") +
  theme(plot.title = element_text(hjust = 0.5))

LDMC_time_plot <-   (LDMC_Driest | LDMC_Dry | LDMC_Wet | LDMC_Wettest)

#ggsave(plot = LDMC_time_plot, filename = "LDMC_temporal_climate.pdf",  width = 30, height = 8, units = "cm")

SLA_alpine <- plot_predictions_time_temp(memodel_data_fullcommunity_nottransformed, "SLA_cm2_g", "mean", "Alpine", SLA_mean_sum_yc)

SLA_sub_alpine <- plot_predictions_time_temp(memodel_data_fullcommunity_nottransformed, "SLA_cm2_g", "mean", "Sub_alpine", SLA_mean_sum_yc)

SLA_boreal <- plot_predictions_time_temp(memodel_data_fullcommunity_nottransformed, "SLA_cm2_g", "mean", "Boreal", SLA_mean_sum_yc)

SLA_time_plot <-   (SLA_alpine/
                    SLA_sub_alpine/
                      SLA_boreal)

ggsave(plot = SLA_time_plot, filename = "SLA_temporal_climate.pdf",  width = 12, height = 27, units = "cm")
  
## Make plot for all trait trends in the spatial climate grid

SLA_space <- plot_predictions_space(memodel_data_fullcommunity_nottransformed, "SLA_cm2_g", "mean", SLA_mean_pred_space) +
  labs(y = "SLA", x = "") +
  guides(color = "none")

LDMC_space <- plot_predictions_space(memodel_data_fullcommunity_nottransformed, "LDMC", "mean", LDMC_mean_pred_space) +
  labs(y = "LDMC", x = "") +
  guides(color = "none")

CN_space <- plot_predictions_space(memodel_data_fullcommunity_nottransformed, "CN_ratio", "mean", CN_ratio_mean_pred_space) +
  labs(y = "Leaf C/N", x = "") +
  guides(color = "none")


LA_space <- plot_predictions_space(memodel_data_fullcommunity_nottransformed, "Leaf_Area_cm2_log", "mean", LA_mean_pred_space) +
  labs(y = "Leaf Area", x = "") +
  guides(color = "none")


C_space <- plot_predictions_space(memodel_data_fullcommunity_nottransformed, "C_percent", "mean", C_mean_pred_space) +
  labs(y = "Leaf C", x = "") +
  guides(color = "none")


N_space <- plot_predictions_space(memodel_data_fullcommunity_nottransformed, "N_percent", "mean", N_mean_pred_space) +
  labs(y = "Leaf N", x = "") +
  guides(color = "none")


Height_space <- plot_predictions_space(memodel_data_fullcommunity_nottransformed, "Plant_Height_mm_log", "mean", Height_mean_pred_space) +
  labs(y = "Plant Height", x = "")  +
  guides(color = "none")

Mass_space <- plot_predictions_space(memodel_data_fullcommunity_nottransformed, "Dry_Mass_g_log", "mean", Mass_mean_pred_space) +
  labs(y = "Leaf Dry Mass", x = "")  +
  guides(color = "none")


Lth_space <- plot_predictions_space(memodel_data_fullcommunity_nottransformed, "Leaf_Thickness_Ave_mm", "mean", Lth_mean_pred_space) +
  labs(y = "Leaf Thickness", x = "")  +
  guides(color = "none")

figure <- ggarrange(LA_space, SLA_space, Height_space, LDMC_space, Mass_space, Lth_space,  C_space, N_space,  CN_space,   nrow = 5, ncol = 2, common.legend = TRUE, legend = "bottom")

#ggsave(plot = figure, filename = "Spatial_trait_trends.pdf",  width = 16, height = 24, units = "cm")


#### Partial R2 figure ####


partR2_plot <- ggplot(aes(x = estimate, y = term, xmin = CI_lower, xmax = CI_upper, color = Space_or_time), data = partR2_data) +
  geom_pointrange() +
  geom_vline(aes(xintercept = Full), color = "black") +
  facet_wrap(~factor(traits, levels = c("SLA_cm2_g",  "LDMC", "Leaf_Thickness_Ave_mm", "Leaf N", "Leaf C/N","Leaf_Area_cm2_log", "Plant_Height_mm_log", "Dry_Mass_g_log", "Leaf C"),), nrow = 2) +
  theme_minimal() +
  xlim(0,0.8) +
  scale_color_manual(values = c("black","#dd7631", "#2E75B6", "chartreuse4")) +
  theme(legend.position = "none", text = element_text(size = 13))

ggsave(plot = partR2_plot, filename = "Partial_R2.pdf",  width = 28, height = 12, units = "cm")


### Spatial vs. temporal ###

time_space_figure <- output_TDT %>% 
  ungroup() %>% 
  #filter(!Trait_trans == "SLA_cm2_g") %>% 
  mutate(effect = case_when(Trait_trans == "SLA_cm2_g" ~ effect/100,
                            Trait_trans == "CN_ratio" ~ effect/10,
                            Trait_trans == "LDMC" ~ effect*10,
                            Trait_trans == "Leaf_Thickness_Ave_mm" ~ effect*10,
                            Trait_trans %in% c("Leaf_Area_cm2_log", "Plant_Height_mm_log", "Dry_Mass_g_log", "C_percent", "N_percent") ~ effect)) %>% 
  mutate(std.error = case_when(Trait_trans == "SLA_cm2_g" ~ std.error/100,
                               Trait_trans == "CN_ratio" ~ std.error/10,
                               Trait_trans == "LDMC" ~ std.error*10,
                               Trait_trans == "Leaf_Thickness_Ave_mm" ~ std.error*10,
                               Trait_trans %in% c("Leaf_Area_cm2_log", "Plant_Height_mm_log", "Dry_Mass_g_log", "C_percent", "N_percent") ~ std.error)) %>% 
  filter(term %in% c("scale(Precip_decade)", "scale(Temp_decade)", "scale(Temp_decade):scale(Precip_decade)", "scale(Temp_annomalies)", "scale(Precip_annomalies)", "scale(Temp_annomalies):scale(Precip_annomalies)")) %>% 
  mutate(term = as.factor(recode(term, "scale(Precip_decade)" = "Precip_space",
                                 "scale(Temp_decade)" = "Temp_space",
                                 "scale(Temp_decade):scale(Precip_decade)" = "Spatial_interaction",
                                 "scale(Precip_annomalies)" = "Precip_time",
                                 "scale(Temp_annomalies)" = "Temp_time",
                                 "scale(Temp_annomalies):scale(Precip_annomalies)" = "Temporal_interaction"))) %>% 
  mutate(climate = case_when(term %in% c("Precip_space", "Precip_time") ~ "BPrecipitation",
                            term %in% c("Temp_space", "Temp_time") ~ "ATemperature",
                            term %in% c("Spatial_interaction", "Temporal_interaction") ~ "CIntercation")) %>% 
  mutate(Time_Space = case_when(term %in% c("Precip_space", "Temp_space", "Spatial_interaction") ~ "Space",
                             term %in% c("Precip_time", "Temp_time", "Temporal_interaction") ~ "ATime")) %>%
  mutate(Significant = case_when(p.value > 0.2 ~ "A_Non_significant",
                                 p.value < 0.2 & p.value > 0.05 ~ "B_Almost_significant",
                                 p.value < 0.05 ~ "C_Significant")) %>%
  #mutate(Trait_moment = paste0(moments, "_", Trait_trans)) %>% 
  #filter(moments %in% c("mean", "time", "Without ITV")) %>% 
  #mutate(moments = factor(moments, levels = c("time", "Without ITV", "mean"))) %>% 
  #mutate(term = as.factor(recode(term, "scale(Precip_decade)" = "Precip_space", "scale(Temp_decade)" = "Temp_space", "scale(Temp_decade):scale(Precip_decade)" = "Spatial_interaction"))) %>% 
  mutate(Trait_trans= (recode(Trait_trans, "SLA_cm2_g" = "SLA_cm2_g/100", "CN_ratio" = "CN_ratio/10", "LDMC" = "LDMC*10", "Leaf_Thickness_Ave_mm" = "Leaf_Thickness_Ave_mm*10"))) %>% 
  mutate(Trait_trans = factor(Trait_trans, levels = c("Leaf_Area_cm2_log", "Plant_Height_mm_log", "Dry_Mass_g_log", "C_percent", "SLA_cm2_g/100", "LDMC*10", "Leaf_Thickness_Ave_mm*10", "N_percent","CN_ratio/10"))) %>%
  ggplot(aes(x = fct_rev(Trait_trans), y = effect, fill = climate, color = Time_Space)) +
  geom_bar(aes(alpha = Significant), stat = "identity", position = position_dodge(width=0.7), width = 0.7) +
  #geom_point(aes(size = if_else(p.value <0.05, 0.3, NA_real_)), position = position_dodge(width=0.6), show.legend = FALSE) +
  #geom_bar_pattern(aes(pattern = 'stripe'), stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = effect-std.error, ymax = effect+std.error), position = position_dodge(width=0.7), width = 0.3) +
  facet_grid(~climate) +
  scale_fill_manual(values = c("#bb3b0e", "#2E75B6",  "#9A86A9")) +
  # scale_fill_gradient(low = "#213964", ##2E75B6
  #                     high = "#BAD8F7",
  #                     guide = "colourbar") +
  scale_color_manual(values = c("black", "black", "black")) +
  #scale_alpha_continuous(range = c(0.9, 0.1)) +
  geom_hline(yintercept =  0) +
  theme_minimal(base_size = 18) +
  theme(axis.title.y=element_blank()) +
  coord_flip()

time_space_figure

ggsave(plot = time_space_figure, filename = "Time_Space_Figure.pdf",  width = 34, height = 16, units = "cm")
