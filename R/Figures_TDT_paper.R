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
library(ggvegan)
library(ggnewscale)
library(gghighlight)
library(merTools)
library(patchwork)
#library(conflicted)
library(broom)

#### Setting conflict standards ####

# conflict_prefer("select", winner = "dplyr")
# conflict_prefer("corrpot", winner = "corrplot")
# conflict_prefer("arm", winner = "corrplot")
# conflict_prefer("filter", winner = "dplyr")

#### Color palettes ####
Temp_palette <- c("#d8c593", "#dd7631", "#bb3b0e")
Precip_palette <- c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")

#### Climate figure ####

## Make the variables for plotting the shift in climate

env_shift <- env %>% 
  group_by(siteID) %>% 
  mutate(shift_temp = Temp_decade - Temp_century,
         shift_precip = Precip_decade - Precip_century) %>% 
  select(siteID, shift_temp, shift_precip) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(mean_shift_temp = mean(shift_temp),
         sd_shift_temp = sd(shift_temp),
         mean_shift_precip = mean(shift_precip),
         sd_shift_precip = sd(shift_precip))


env %>% 
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

#ggsave("Temperature_over_time.png", width = 20 , height = 11, units = "cm")


env %>% 
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

#ggsave("Precipitation_over_time.png", width = 22 , height = 11, units = "cm")


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
  scale_shape_manual(name = "Temperature", values = c(2, 24, 6, 25, 1, 21)) + 
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

corr <- round(cor(Trait_climate_corr), 1) 
corr1 <- round(cor(Climate_corr, use = "complete.obs"), 2) 

# P-values 
p.mat <- cor_pmat(Corr_traits)
#head(p.mat_17[, 1:4])

# Correlation plot

ggcorrplot(corr, hc.order = FALSE,
           type = "lower", lab = TRUE,)

#ggsave("Correlation_plot.pdf", width = 22 , height = 16, units = "cm")

ggcorrplot(corr1, hc.order = FALSE,
           type = "lower", lab = TRUE,)

#ggsave("Correlation_plot_climate.pdf", width = 20 , height = 15, units = "cm")


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
  xlab("PCA1 (43.9%)") + #Numbers added manually from the fviz_eig plot above
  ylab("PCA2 (19.0%)") + #Numbers added manually from the fviz_eig plot above
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
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_blank(), axis.title = element_blank()) +
  labs(title = "", fill = "Summer temperature", color = "Summer temperature", shape = "Summer temperature") +
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
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_blank(), axis.title = element_blank()) +
  labs(title = "", fill = "Yearly precipitation", color = "Yearly precipitation", shape = "Yearly precipitation") +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.1), legend.position = 'top')

#ggsave("Ordination_Precip.svg", width = 18 , height = 11, units = "cm", dpi = 600)

## Ordination over time ##

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
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1), axis.title = element_blank()) +
  labs(fill = "Yearly precipitation", color = "Yearly precipitation", shape = "Yearly precipitation") +
  guides(color = "none") +
  scale_shape_manual(values = c(16, 17, 15))


c <- ggarrange(Ord_plot_traits, Ord_plot_time, Ord_plot_precip, Ord_plot_temp,   
               nrow = 2, ncol = 2,
               labels = c("A)","B)", "C)", "D)"),
               legend = "none")

#ggsave(plot = c, "Ord_timemean_temp_prec_four.pdf", width = 26 , height = 18, units = "cm")

### RDA variance explained ordination plot ###

Explained_variance_RDA <- read.table(header = TRUE, stringsAsFactors = TRUE, text = 
                                       "x model variance
                               1 Temperature 0.2288
                               1 Precipitation 0.1523
                               1 Temp*Precip 0.1788
                               1 Temp*Precip*Year 0.0187
                               1 Unexplained 0.4214
                               ")

Ord_RDA <- Explained_variance_RDA %>% 
  mutate(model = factor(model, levels = c("Unexplained", "Temp*Precip*Year", "Temp*Precip",  "Temperature", "Precipitation"))) %>% 
  ggplot(aes(x = x, fill = model, y = variance)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#CCCBCC", "purple", "mediumpurple1", "#dd7631", "#2E75B6")) +
  theme_minimal(base_size = 18) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom') +
  ylab("Variance explained")


#ggsave(filename = "Ordination_RDA.jpg",  width = 20, height = 14, units = "cm")

#### Mixed effect model plots ####

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
env %>%  select(siteID, Temp_annomalies) %>%
  mutate(Temp_annomalies = abs(Temp_annomalies)) %>% 
  group_by(siteID) %>% 
  mutate(Temp = mean(Temp_annomalies)) %>%
  select(-Temp_annomalies) %>%
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

SLA_Driest <- plot_predictions_time(memodel_data_fullcommunity_nottransformed, "SLA_cm2_g", "mean", "Driest", SLA_mean_sum_yc,"Temp_annomalies") +
  labs(y = "",  title = "Driest") +
  theme(plot.title = element_text(hjust = 0.5))

SLA_Dry <- plot_predictions_time(memodel_data_fullcommunity_nottransformed, "SLA_cm2_g", "mean", "Dry", SLA_mean_sum_yc,"Precip_annomalies") +
  labs(y = "",  title = "Dry") +
  theme(plot.title = element_text(hjust = 0.5))

SLA_Wet <- plot_predictions_time(memodel_data_fullcommunity_nottransformed, "SLA_cm2_g", "mean", "Wet", SLA_mean_sum_yc, "Precip_annomalies") +
  labs(y = "",  title = "Wet") +
  theme(plot.title = element_text(hjust = 0.5))

SLA_Wettest <- plot_predictions_time(memodel_data_fullcommunity_nottransformed, "SLA_cm2_g", "mean", "Wettest", SLA_mean_sum_yc, "Precip_annomalies") +
  labs(y = "",  title = "Wettest") +
  theme(plot.title = element_text(hjust = 0.5))

SLA_time_plot <-   (SLA_Driest | SLA_Dry | SLA_Wet | SLA_Wettest)
  
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
  facet_wrap(~factor(traits, levels = c("SLA_cm2_g", "Leaf N", "Leaf C/N", "LDMC", "Leaf_Thickness_Ave_mm", "Leaf_Area_cm2_log", "Plant_Height_mm_log", "Dry_Mass_g_log", "Leaf C"),), nrow = 2) +
  theme_minimal() +
  xlim(0,0.8) +
  scale_color_manual(values = c("black","#dd7631", "#2E75B6", "chartreuse4")) +
  theme(legend.position = "none", text = element_text(size = 13))

ggsave(plot = partR2_plot, filename = "Partial_R2.pdf",  width = 28, height = 12, units = "cm")


### Spatial vs. temporal ###

output_TDT %>% 
  mutate(term = dplyr::recode(term, "scale(Temp_decade)" = "Temp_decade",
                        "scale(Temp_annomalies)" = "Temp_anomalies")) %>%
  ungroup() %>% 
  dplyr::select(Trait_trans, term, effect) %>% 
  pivot_wider(names_from = term, values_from = effect) %>%
  ggplot(aes(x = Temp_decade, y = Temp_anomalies, color = Trait_trans)) + 
  geom_point() +
  theme_minimal() +
  xlim(-0.015,0.015) +
  ylim(-0.015, 0.015) +
  geom_abline(slope = 1)

output_TDT %>% 
  mutate(term = dplyr::recode(term, "scale(Precip_decade)" = "Precip_decade",
                              "scale(Precip_annomalies)" = "Precip_anomalies")) %>%
  ungroup() %>% 
  dplyr::select(Trait_trans, term, effect) %>% 
  mutate(effect = 10^effect) %>% 
  pivot_wider(names_from = term, values_from = effect) %>%
  ggplot(aes(x = Precip_decade, y = Precip_anomalies, color = Trait_trans)) + 
  geom_point() +
  theme_minimal() +
  #xlim(0.9975,1.0025) +
  #ylim(0.995, 1.004) +
  #xlim(-0.00082,0.00082) +
  #ylim(-0.00082, 0.00082) +
  geom_abline(slope = 1)



output_TDT %>% 
  mutate(term = dplyr::recode(term, "scale(Temp_decade):scale(Precip_decade)" = "Spatial_interaction",
                              "scale(Temp_annomalies):scale(Precip_annomalies)" = "Temporal_interaction")) %>%
  ungroup() %>% 
  dplyr::select(Trait_trans, term, effect) %>% 
  pivot_wider(names_from = term, values_from = effect) %>%
  ggplot(aes(x = Spatial_interaction, y = Temporal_interaction, color = Trait_trans)) + 
  geom_point() +
  theme_minimal() +
  xlim(-0.015,0.015) +
  ylim(-0.015, 0.015) +
  geom_abline(slope = 1)


output_TDT %>% 
  ungroup() %>% 
  filter(!Trait_trans == "SLA_cm2_g") %>% 
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
  #mutate(Trait_moment = paste0(moments, "_", Trait_trans)) %>% 
  #filter(moments %in% c("mean", "time", "Without ITV")) %>% 
  #mutate(moments = factor(moments, levels = c("time", "Without ITV", "mean"))) %>% 
  #mutate(term = as.factor(recode(term, "scale(Precip_decade)" = "Precip_space", "scale(Temp_decade)" = "Temp_space", "scale(Temp_decade):scale(Precip_decade)" = "Spatial_interaction"))) %>% 
  mutate(Trait_trans = factor(Trait_trans, levels = c("Leaf_Area_cm2_log", "Plant_Height_mm_log", "Dry_Mass_g_log", "C_percent", "SLA_cm2_g", "N_percent", "CN_ratio", "LDMC", "Leaf_Thickness_Ave_mm"))) %>%
  ggplot(aes(x = fct_rev(Trait_trans), y = effect, fill = climate, color = Time_Space)) +
  geom_bar(aes(alpha = rev(p.value)), stat = "identity", position = position_dodge(width=0.7), width = 0.7) +
  #geom_point(aes(size = if_else(p.value <0.05, 0.3, NA_real_)), position = position_dodge(width=0.6), show.legend = FALSE) +
  #geom_bar_pattern(aes(pattern = 'stripe'), stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = effect-std.error, ymax = effect+std.error), position = position_dodge(width=0.7), width = 0.3) +
  facet_grid(~climate) +
  scale_fill_manual(values = c("#bb3b0e", "#2E75B6",  "#9A86A9")) +
  # scale_fill_gradient(low = "#213964", ##2E75B6
  #                     high = "#BAD8F7",
  #                     guide = "colourbar") +
  scale_color_manual(values = c("black", "black", "black")) +
  scale_alpha_continuous(range = c(0.9, 0.1)) +
  geom_hline(yintercept =  0) +
  theme_bw(base_size = 18) +
  coord_flip() +
  #guides(fill = "none", color = "none", size = "none") +
  theme(axis.title.y=element_blank()) 
  #guides(color = FALSE, alpha = FALSE, fill = FALSE)
