ClimNorway <- metaNorway %>% 
  mutate(Temp = recode(Site, Ulv=6.17, Lav=6.45, Gud=5.87, Skj=6.58, Alr=9.14, Hog=9.17, Ram=8.77, Ves=8.67, Fau=10.3, Vik=10.55, Arh=10.60, Ovs=10.78))%>%
  mutate(Precip= recode(Site, Ulv=596, Lav=1321, Gud=1925, Skj=2725, Alr=789, Hog=1356, Ram=1848, Ves=3029, Fau=600, Vik=1161, Arh=2044, Ovs=2923)) %>% 
  rename("Temp_summer" = "bio10", "Precip_annual" = "bio12") %>% 
  select(Site, VPD, Temp_summer, Precip_annual)

ggplot(aes(VPD, Temp_summer, col = Precip_annual), data = ClimNorway) +
  geom_point(size = 4) +
  labs(col = "Precipitation (cm)", y = "Summer temperature", x = "Vaper pressure deficit" ) +
  scale_color_gradient(low = "deepskyblue", high = "#000033") +
  theme_bw()

corr_clim <- round(cor(ClimNorway[, -1]), 1)

p.mat <- cor_pmat(ClimNorway[, -1])

ggcorrplot(corr_clim, hc.order = TRUE,
           type = "lower", p.mat = p.mat, lab = TRUE)
