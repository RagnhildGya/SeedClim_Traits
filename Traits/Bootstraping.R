### Log transforming ###
library(e1071)

### Bootstraping the community weighted means and other things ###

CWM_Bootstrapping <- function(community, trait, nrep = 100, samplesize = 200, moments = TRUE){
  comm <- community %>% 
    filter(!cover == 0)
  
  t_dat <- trait %>% 
    filter(!is.na(Value)) %>% 
    select(-Trait) %>% 
    rename("Trait" = "Trait_trans")
  
  TraitWeights <- comm %>% 
    left_join(t_dat, by = c("Site" = "Site", "species" = "Species")) %>% 
    group_by(Site, Block, turfID, Trait) %>% 
    mutate(n = n())

  
  BootstrapMoments_All <- map_df(
    1:nrep,
    ~{x <- sample_n(TraitWeights, replace = TRUE, size = samplesize, weight = cover_species)
    
    if (moments){
      x <- x %>% summarise(Mean = mean(Value), Variance = var(Value), Skewness = skewness(Value), Kurtosis = kurtosis(Value))

    }
x 
})
  
  return(BootstrapMoments_All)
}

SummarizeBootMoments <- function(BootstrapMoments_All){
  # calculate means and confidence intervals
  BootstrapMoments <- BootstrapMoments_All %>% 
    group_by(Site, Block, turfID, Trait) %>% 
    summarise(n = n(),
              meanMean = mean(Mean), CIlow.Mean = meanMean - sd(Mean), CIhigh.Mean = meanMean + sd(Mean),
              meanVar = mean(Variance), CIlow.Var = meanVar - sd(Variance), CIhigh.Var = meanVar + sd(Variance),
              meanSkew = mean(Skewness), CIlow.Skew = meanSkew - sd(Skewness), CIhigh.Skew = meanSkew + sd(Skewness),
              meanKurt = mean(Kurtosis), CIlow.Kurt = meanKurt - sd(Kurtosis), CIhigh.Kurt = meanKurt + sd(Kurtosis)) 
  
  return(BootstrapMoments)
}

# Model_Bootstrapping <- function(Bootstrap_Traits, nrep = 100, samplesize = 200, moments = TRUE){
#   
#   Bootstrap_Traits1 <- Bootstrap_Traits %>% 
#   mutate(T_level = recode(Site, Ulv = 6.5, Lav = 6.5,  Gud = 6.5, Skj = 6.5, Alr = 8.5, Hog = 8.5, Ram = 8.5, Ves = 8.5, Fau = 10.5, Vik = 10.5, Arh = 10.5, Ovs = 10.5)) %>%
#     mutate(Temp = recode(Site, Ulv=6.17, Lav=6.45, Gud=5.87, Skj=6.58, Alr=9.14, Hog=9.17, Ram=8.77, Ves=8.67, Fau=10.3, Vik=10.55, Arh=10.60, Ovs=10.78))%>%
#     mutate(Precip= recode(Site, Ulv=596, Lav=1321, Gud=1925, Skj=2725, Alr=789, Hog=1356, Ram=1848, Ves=3029, Fau=600, Vik=1161, Arh=2044, Ovs=2923))%>%
#     mutate(P_level = recode(Site, Ulv = 600, Alr = 600, Fau = 600, Lav = 1200, Hog = 1200, Vik = 1200, Gud = 2000, Ram = 2000, Arh = 2000, Skj = 2700, Ves = 2700, Ovs = 2700)) %>% 
#     filter(!is.na(Trait)) %>% 
#     ungroup() %>% 
#     gather(Moment, Value, Mean, Variance, Skewness, Kurtosis) %>% 
#     select(Trait, Moment, Site, turfID, Temp, Precip, Value) %>% 
#     group_by(Trait, Moment, turfID) %>% 
#     mutate(n = 1:n()) %>% 
#     ungroup() %>%
#     select(-turfID) %>% 
#     group_by(Trait, Moment, n) %>% 
#     nest()
#   
#   Bootstrap_Traits2 <- Bootstrap_Traits1
#     mutate(fit = map_df(Bootstrap_Traits1,
#                         1:nrep,
#                         ~ {lmer(Value ~ Temp + scale(Precip) + Temp:scale(Precip) + (1 | Site), data = .x)})) %>% 
#     mutate(model_output = map(model, tidy)) %>% 
#     select(Trait, Moment, model_output) %>% 
#     unnest()
#   
#   
#   Bootstrap_Model <- map_df(Bootstrap_Traits1, 
#     1:nrep,
#     ~{model <- lmer(Value ~ Temp + scale(Precip) + Temp:scale(Precip) + (1 | Site), data = Bootstrap_Traits1)
#     })
#     
#     Model_summary <- Bootstrap_Model %>%
#       mutate(model_output = map(model, tidy)) %>% 
#       select(Trait, Moment, model_output) %>% 
#       unnest()
#     
#     })
#   
#   return(Bootstrap_Model)
# }

# #Site level weights and traits  
#   
#   comm <- community_cover
#   t_dat <- traitdata_1
#   
#   TraitWeights <- comm %>% 
#     left_join(t_dat, by = c("Site" = "Site", "species" = "Species")) %>% 
#     group_by(Site, Block, turfID, Trait)
#   
#   
#   # Regional level weights and traits
#   TraitWeights_regional <- comm %>% 
#     left_join(trait, by = c("species" = "Species")) %>% 
#     group_by(Country, Year, Gradient, Taxon, Trait) %>% 
#     mutate(weight = Cover/n()) %>% 
#     group_by(Country, Year, Gradient, Trait) 
#   
#   
#   TraitWeights_all <- bind_rows(plot = TraitWeights_plot, site = TraitWeights_site, global = TraitWeights_global, .id = "level") %>% 
#     mutate(level = factor(level, levels = c("plot", "site", "global"), ordered = TRUE)) %>%
#     filter(!is.na(Value)) %>% 
#     group_by(Country, Year, Site, Gradient, BlockID, PlotID, Trait, Taxon) %>% 
#     filter(level == min(level)) %>% 
#     group_by(Country, Year, Site, Gradient, BlockID, PlotID, Trait)
#   

