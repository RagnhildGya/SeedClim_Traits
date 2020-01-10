### Make multiple plots with Skewness
library(ggpubr)

### Raw data bootstrapped - visualizing the distirbutions we calculate mean, variance, skewness and kurtosis on ###

Raw_Data_Weighted %>% 
  filter(Trait %in% c("CN_ratio", "LDMC",  "Plant_Height_mm_log", "SLA_cm2_g")) %>% 
  ggplot(aes(Value, fill = as.factor(T_level))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Trait, scales = "free") +
  labs(fill = "Summer temperature (C)") +
  theme_bw() 
#scale_fill_manual(values = c("cyan3", "darkolivegreen","goldenrod2" "red"))

Raw_Data_Weighted %>% 
  filter(Trait %in% c("Leaf_Thickness_Ave_mm",  "Plant_Height_mm_log")) %>% 
  ggplot(aes(Value, fill = as.factor(P_level))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Trait, scales = "free") +
  labs(fill = "Precipitation (mm/year)") +
  theme_bw() 

Raw_Data_Weighted %>% 
  filter(Trait %in% c("Leaf_Thickness_Ave_mm",  "Plant_Height_mm_log")) %>% 
  ggplot(aes(Value, fill = as.factor(T_level))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Trait, scales = "free") +
  labs(fill = "Summer temperature (C)") +
  theme_bw() 



#ggsave("Raw_data_distributions.jpg", width = 25 , height = 15, units = "cm")

### Plotting the significant trends from the test ###

#Make a function for plotting this

# plot_skewness<-function(df) {
# 
#   df %>% 
#     ggplot(aes(Precip, meanSkew, color = T_cat)) +
#     geom_point(alpha = 0.5) +
#     geom_errorbar(aes(ymin = CIlow.Skew, ymax = CIhigh.Skew)) +
#     labs(y = "Skewness - Dry mass (g)")+
#     theme_bw()
# }

## Mean

CI_Mean_Boot_Traits %>% 
  ggplot(aes(VPD, meanMean, color = T_cat, shape = P_cat)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Mean, ymax = CIhigh.Mean)) +
  facet_wrap(~Trait, nrow = 3, scales = "free_y") +
  labs(y = "Community weighted Mean", x = "Vapour pressure deficit")+
  theme_bw()

## Variance

CI_Mean_Boot_Traits %>% 
  ggplot(aes(VPD, meanVar, color = T_cat, shape = P_cat)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Var, ymax = CIhigh.Var)) +
  facet_wrap(~Trait, nrow = 3, scales = "free_y") +
  labs(y = "Community weighted Variance", x = "Vapour pressure deficit")+
  theme_bw()

## Kurtosis

CI_Mean_Boot_Traits1 %>% 
  ggplot(aes(VPD, meanKurt, color = T_cat)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Kurt, ymax = CIhigh.Kurt)) +
  facet_wrap(~Trait, nrow = 3, scales = "free_y") +
  labs(y = "Community weighted Kurtosis", x = "Vapour pressure deficit")+
  theme_bw()

## Skewness

CI_Mean_Boot_Traits1 %>% 
  ggplot(aes(VPD, meanSkew, color = T_cat)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Skew, ymax = CIhigh.Skew)) +
  facet_wrap(~Trait, nrow = 3, scales = "free_y") +
  labs(y = "Community weighted Skewness", x = "Vapour pressure deficit")+
  theme_bw()



## Probability distribution of the community weighted mean of SLA ##

Bootstrap_Traits1 %>% 
  filter(Trait == "SLA_cm2_g") %>% 
  ggplot(aes(Mean, fill = T_cat))+
  geom_density(alpha = 0.5)+
  #facet_wrap(~P_cat, nrow = 1) +
  labs(x = "Community weighted means - SLA (cm2/g)", fill = "Temperature category", title = "Porbability distribution of the community weighted mean of SLA")+
  theme_bw()+
  scale_fill_manual(values = c("#99CCFF", "#9999FF", "#FF9999"))

#Plot showing the (probability distribution of) community weighted mean in points for four chosen traits

# Bootstrap_Traits2 %>% 
#   ungroup() %>% 
#   mutate(Moment = factor(Moment, levels = c("Mean", "Variance", "Skewness", "Kurtosis"))) %>% 
#   filter(Trait %in% c("CN_ratio", "Plant_Height_mm_log", "SLA_cm2_g", "LDMC")) %>% 
#   unnest(data) %>% 
#   ggplot(aes(Temp, Value, col = Precip)) +
#   geom_jitter()+
#   facet_grid(Moment ~ Trait, scales = "free_y")


#ggsave("Bootstrap_SLA_mean.jpg", width = 20 , height = 15, units = "cm")

## Variance, Kurtosis, Skewness ##

Bootstrap_Traits1 %>% 
  filter(Trait == "SLA_cm2_g") %>% 
  ggplot(aes(Variance, fill = T_cat))+
  geom_density(alpha = 0.5)+
  #facet_wrap(~P_cat, nrow = 1) +
  labs(x = "Community weighted variance - SLA (cm2/g)", fill = "Temperature category")+
  theme_bw()

Bootstrap_Traits1 %>% 
  filter(Trait == "Plant_Height_mm_log") %>% 
  ggplot(aes(Skewness, fill = T_cat))+
  geom_density(alpha = 0.5)+
  #facet_wrap(~P_cat, nrow = 1) +
  labs(x = "Community weighted skewness - SLA (cm2/g)", fill = "Temperature category")+
  theme_bw()+
  scale_fill_manual(values = c("#FFDB6D", "#C3D7A4", "#4E84C4", "#3333CC"))

Bootstrap_Traits1 %>% 
  filter(Trait == "SLA_cm2_g") %>% 
  ggplot(aes(Kurtosis, fill = T_cat))+
  geom_density(alpha = 0.5)+
  #facet_wrap(~P_cat, nrow = 1) +
  labs(x = "Community weighted kurtosis - SLA (cm2/g)", fill = "Temperature category")+
  theme_bw()+
  geom_vline(xintercept = 1.2)


## Mean of all traits ##

Bootstrap_Traits1 %>% 
  filter(Trait %in% c("CN_ratio", "LDMC",  "Plant_Height_mm_log", "SLA_cm2_g")) %>% 
  ggplot(aes(Mean, fill = T_cat))+
  geom_density(alpha = 0.5)+
  facet_wrap(~Trait, scales = "free") +
  labs(x = "Community weighted means", fill = "Temperature category")+
  theme_bw()+ 
  scale_fill_manual(values = c("#99CCFF", "#9999FF", "#FF9999"))


#ggsave("Bootstrap_all_traits_mean.jpg", width = 20 , height = 15, units = "cm")


### Kurtosis vs. skewness ##

Bootstrap_Traits1 %>% 
  #filter(Trait == "SLA_cm2_g") %>% 
  ggplot(aes(x = Skewness, y = Kurtosis, col = T_cat))+
  geom_point() +
  theme_bw()

#ggsave("Nikeplot_all_traits.jpg", width = 20 , height = 15, units = "cm")



## Making some trial plot of the predicted values

predicted_values %>% 
  select(Trait, Moment, Temp, Precip, modeled, measured) %>% 
  filter(Trait == "SLA_cm2_g") %>% 
  ggplot(aes(Temp, measured, col = Precip)) + 
  geom_jitter() +
  geom_line(aes(y = modeled)) + 
  facet_wrap(~Moment, scales = "free")

predicted_values %>% 
  filter(Trait == "SLA_cm2_g") %>%
  filter(Moment == "Mean") %>% 
  pivot_longer(measured:modeled, names_to = "Type", values_to = "Value") %>% 
  distinct() %>% 
  #stack measured and modeled, name it value, and color by type (measured of modeled)
  ggplot(aes(x = Temp, y = Value)) +
  geom_jitter(aes(col = Type), width = 0.1)


## Plot of significant trends ##

Summary_model_significance %>% 
  ungroup() %>% 
  filter(term == "(Intercept)",
         Moment %in% c("Skewness", "Kurtosis")) %>% 
  mutate(Trait = factor(Trait, levels = c("C_percent", "N_percent", "CN_ratio", "Plant_Height_mm_log", "Leaf_Area_cm2_log", "Leaf_Thickness_Ave_mm", "Dry_Mass_g_log", "Wet_Mass_g_log", "SLA_cm2_g", "LDMC"))) %>% 
  ggplot(aes(Trait, mean.fit, col = Significant)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.fit, ymax = CIhigh.fit)) +
  geom_hline(yintercept = 0, col = "black") +
  coord_flip() +
  facet_wrap(~Moment, scales = "free_x", nrow = 1) +
  labs(y = "Estimate of model")+
  theme_bw() +
  scale_color_manual(values = c("Red", "Black", "Blue"))


#ggsave("Intercept_Kurtosis_Skewness.jpg", width = 20 , height = 13, units = "cm")


model_output_graph_temp <- model_output %>% 
  ungroup() %>% 
  filter(term == "Temp") %>% 
  mutate(Moment = factor(Moment, levels = c("Mean", "Variance", "Skewness", "Kurtosis"))) %>% 
  mutate(Trait = factor(Trait, levels = c("C_percent", "N_percent", "CN_ratio", "Plant_Height_mm_log", "Leaf_Area_cm2_log", "Leaf_Thickness_Ave_mm", "Dry_Mass_g_log", "Wet_Mass_g_log", "SLA_cm2_g", "LDMC"))) %>% 
  # mutate(effect = scale(effect, center = 0),
  #        CIlow.fit = scale(CIlow.fit, center = 0),
  #        CIhigh.fit = scale(CIhigh.fit, center = 0)) %>% 
  ggplot(aes(Trait, effect, col = Significant)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.fit, ymax = CIhigh.fit)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap(~Moment, scales = "free_x", nrow = 1) +
  labs(y = "Estimate of model")+
  theme_bw() +
  scale_color_manual(values = c("Blue", "Black", "Red"))

model_output_graph_precip <- model_output %>% 
  ungroup() %>% 
  filter(term == "scale(Precip)") %>% 
  mutate(Moment = factor(Moment, levels = c("Mean", "Variance", "Skewness", "Kurtosis"))) %>% 
  mutate(Trait = factor(Trait, levels = c("C_percent", "N_percent", "CN_ratio", "Plant_Height_mm_log", "Leaf_Area_cm2_log", "Leaf_Thickness_Ave_mm", "Dry_Mass_g_log", "Wet_Mass_g_log", "SLA_cm2_g", "LDMC"))) %>% 
  ggplot(aes(Trait, effect, col = Significant)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.fit, ymax = CIhigh.fit)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap(~Moment, scales = "free_x", nrow = 1) +
  labs(y = "Estimate of model")+
  theme_bw() +
  scale_color_manual(values = c("Blue", "Black", "Red"))



############ Old code - can this be used to something, or should it just be removed? ###########


## Skewness Leaf thickness ##

CI_Mean_Boot_Traits2 <- CI_Mean_Boot_Traits1 %>% 
  group_by(Trait) %>% 
  mutate(M_meanMean = mean(meanMean),
         CIlow_M_Mean = M_meanMean - sd(meanMean),
         CIhigh_M_Mean = M_meanMean + sd(meanMean),
         M_meanVar = mean(meanVar),
         CIlow_M_Var = M_meanVar - sd(meanVar),
         CIhigh_M_Var = M_meanVar + sd(meanVar),
         M_meanKurt = mean(meanKurt),
         CIlow_M_Kurt = M_meanKurt - sd(meanKurt),
         CIhigh_M_Kurt = M_meanKurt + sd(meanKurt),
         M_meanSkew = mean(meanSkew),
         CIlow_M_Skew = M_meanSkew - sd(meanSkew),
         CIhigh_M_Skew = M_meanSkew + sd(meanSkew))

Lth_skew <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "Leaf_Thickness_Ave_mm") %>% 
  ggplot(aes(VPD, meanSkew)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Skew, ymax = CIhigh.Skew)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(y = M_meanSkew, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Skew, ymax=CIhigh_M_Skew, fill = "red"), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Leaf thickness (mm)") +
  coord_flip()

## Skewness Leaf area ##

LA_Skew <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "Leaf_Area_cm2_log") %>% 
  ggplot(aes(VPD, meanSkew)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Skew, ymax = CIhigh.Skew)) +
  labs(y = "Leaf area (cm2)")+
  geom_hline(yintercept = 0) +
  geom_line(aes(y = M_meanSkew, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Skew, ymax=CIhigh_M_Skew, fill = "red"), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none")+
  coord_flip()

Dry_Skew <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "Dry_Mass_g_log") %>% 
  ggplot(aes(VPD, meanSkew)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Skew, ymax = CIhigh.Skew)) +
  labs(y = "Dry mass (g)")+
  geom_hline(yintercept = 0) +
  geom_line(aes(y = M_meanSkew, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Skew, ymax=CIhigh_M_Skew, fill = "red"), alpha = 0.3) +
  theme_bw()+
  theme(legend.position = "none") +
  coord_flip()
  

CN_Skew <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "CN_ratio") %>% 
  ggplot(aes(VPD, meanSkew)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Skew, ymax = CIhigh.Skew)) +
  labs(y = "C/N ratio")+
  geom_hline(yintercept = 0) +
  geom_line(aes(y = M_meanSkew, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Skew, ymax=CIhigh_M_Skew, fill = "red"), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()

C_Skew <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "C_percent") %>% 
  ggplot(aes(VPD, meanSkew)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Skew, ymax = CIhigh.Skew)) +
  labs(y = "C %")+
  geom_hline(yintercept = 0) +
  geom_line(aes(y = M_meanSkew, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Skew, ymax=CIhigh_M_Skew, fill = "red"), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()

N_Skew <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "N_percent") %>% 
  ggplot(aes(VPD, meanSkew)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Skew, ymax = CIhigh.Skew)) +
  labs(y = "N %")+
  geom_hline(yintercept = 0) +
  geom_line(aes(y = M_meanSkew, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Skew, ymax=CIhigh_M_Skew, fill = "red"), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()

SLA_Skew <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "SLA_cm2_g") %>% 
  ggplot(aes(VPD, meanSkew)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Skew, ymax = CIhigh.Skew)) +
  labs(y = "SLA (cm2/g)")+
  geom_hline(yintercept = 0) +
  geom_line(aes(y = M_meanSkew, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Skew, ymax=CIhigh_M_Skew, fill = "red"), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()

Height_Skew <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "Plant_Height_mm_log") %>% 
  ggplot(aes(VPD, meanSkew)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Skew, ymax = CIhigh.Skew)) +
  labs(y = "Height (mm)")+
  geom_hline(yintercept = 0) +
  geom_line(aes(y = M_meanSkew, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Skew, ymax=CIhigh_M_Skew, fill = "red"), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()

LDMC_Skew <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "LDMC") %>% 
  ggplot(aes(VPD, meanSkew)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Skew, ymax = CIhigh.Skew)) +
  labs(y = "LDMC")+
  geom_hline(yintercept = 0) +
  geom_line(aes(y = M_meanSkew, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Skew, ymax=CIhigh_M_Skew, fill = "red"), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()


Skew_fig <- ggarrange(LDMC_Skew, SLA_Skew, CN_Skew, C_Skew, N_Skew, Lth_skew, LA_Skew, Dry_Skew, Height_Skew, 
          ncol = 3, nrow = 3)

annotate_figure(Skew_fig, top = text_grob("Skewness", face = "bold", size = 14))

### Make multiple plots with Kurtosis

Lth_Kurt <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "Leaf_Thickness_Ave_mm") %>% 
  ggplot(aes(VPD, meanKurt)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Kurt, ymax = CIhigh.Kurt)) +
  geom_hline(yintercept = 0) +
  labs(y = "Leaf thickness (mm)") +
  geom_line(aes(y = M_meanKurt, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Kurt, ymax=CIhigh_M_Kurt, fill = "red"), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()



LA_Kurt <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "Leaf_Area_cm2_log") %>% 
  ggplot(aes(VPD, meanKurt)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Kurt, ymax = CIhigh.Kurt)) +
  labs(y = "Leaf area (cm2)")+
  geom_hline(yintercept = 0) +
  geom_line(aes(y = M_meanKurt, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Kurt, ymax=CIhigh_M_Kurt, fill = "red"), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()

Dry_Kurt <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "Dry_Mass_g_log") %>% 
  ggplot(aes(VPD, meanKurt)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Kurt, ymax = CIhigh.Kurt)) +
  labs(y = "Dry mass (g)")+
  geom_hline(yintercept = 0) +
  geom_line(aes(y = M_meanKurt, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Kurt, ymax=CIhigh_M_Kurt, fill = "red"), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()

CN_Kurt <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "CN_ratio") %>% 
  ggplot(aes(VPD, meanKurt)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Kurt, ymax = CIhigh.Kurt)) +
  labs(y = "C/N ratio")+
  geom_hline(yintercept = 0) +
  geom_line(aes(y = M_meanKurt, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Kurt, ymax=CIhigh_M_Kurt, fill = "red"), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()

C_Kurt <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "C_percent") %>% 
  ggplot(aes(VPD, meanKurt)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Kurt, ymax = CIhigh.Kurt)) +
  labs(y = "C %")+
  geom_hline(yintercept = 0) +
  geom_line(aes(y = M_meanKurt, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Kurt, ymax=CIhigh_M_Kurt, fill = "red"), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()

N_Kurt <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "N_percent") %>% 
  ggplot(aes(VPD, meanKurt)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Kurt, ymax = CIhigh.Kurt)) +
  labs(y = "N %")+
  geom_hline(yintercept = 0) +
  geom_line(aes(y = M_meanKurt, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Kurt, ymax=CIhigh_M_Kurt, fill = "red"), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()

SLA_Kurt <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "SLA_cm2_g") %>% 
  ggplot(aes(VPD, meanKurt)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Kurt, ymax = CIhigh.Kurt)) +
  labs(y = "SLA (cm2/g)")+
  geom_hline(yintercept = 0) +
  geom_line(aes(y = M_meanKurt, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Kurt, ymax=CIhigh_M_Kurt, fill = "red"), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()

Height_Kurt <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "Plant_Height_mm_log") %>% 
  ggplot(aes(VPD, meanKurt)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Kurt, ymax = CIhigh.Kurt)) +
  labs(y = "Height (mm)")+
  geom_hline(yintercept = 0) +
  geom_line(aes(y = M_meanKurt, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Kurt, ymax=CIhigh_M_Kurt, fill = "red"), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()

LDMC_Kurt <- CI_Mean_Boot_Traits2 %>% 
  filter(Trait == "LDMC") %>% 
  ggplot(aes(VPD, meanKurt)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = CIlow.Kurt, ymax = CIhigh.Kurt)) +
  labs(y = "LDMC")+
  geom_hline(yintercept = 0) +
  geom_line(aes(y = M_meanKurt, col = "red")) +
  geom_ribbon(aes(ymin=CIlow_M_Kurt, ymax=CIhigh_M_Kurt, fill = "red"), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()


Kurt_fig <- ggarrange(LDMC_Kurt, SLA_Kurt, CN_Kurt, C_Kurt, N_Kurt, Lth_Kurt, LA_Kurt, Dry_Kurt, Height_Kurt, 
          ncol = 3, nrow = 3)

annotate_figure(Kurt_fig, top = text_grob("Kurtosis", face = "bold", size = 14))
                