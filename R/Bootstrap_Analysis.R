#### Run analysis ####

### Source ###

source("R/Cleaning.R")
#source("R/Bootstraping.R")

### Libraries ###

# library(broom.mixed)
# library(lme4)
# library(lmerTest)
# library(purrr)
# library(piecewiseSEM)
# library(factoextra)
# library(GGally)
# library(ggcorrplot)
# library(textshape)
library(traitstrap)

set.seed(47)

## Trying traitstrap ##

community2009 <- community %>% 
  filter(!is.na(cover9))

community2017 <- community %>% 
  filter(!is.na(cover17))

traitdata_2 <- traitdata_1 %>% 
  mutate(blockID = "",
         turfID = "")

SeedClim_traits_2009 <- trait_impute(comm = community2009,
                                traits = traitdata_2, 
                                scale_hierarchy = c("Site", "blockID", "turfID"),
                                global = FALSE,
                                taxon_col = c("Full_name", "Genus", "Family"),
                                trait_col = "Trait_trans",
                                value_col = "Value",
                                abundance_col = "cover9")

SeedClim_traits_2017 <- trait_impute(comm = community2017,
                                     traits = traitdata_2, 
                                     scale_hierarchy = c("Site", "blockID", "turfID"),
                                     global = FALSE,
                                     taxon_col = c("Full_name", "Genus", "Family"),
                                     trait_col = "Trait_trans",
                                     value_col = "Value",
                                     abundance_col = "cover17")


SC_moments_2009 <- trait_np_bootstrap(imputed_traits = SeedClim_traits_2009, nrep = 100)
SC_moments_2017 <- trait_np_bootstrap(imputed_traits = SeedClim_traits_2017, nrep = 100)

sum_SC_moments_2009 = trait_summarise_boot_moments(SC_moments_2009)
sum_SC_moments_2017 = trait_summarise_boot_moments(SC_moments_2017)



summarised_boot_moments_climate_2009 = bind_rows(
  sum_SC_moments_2009 %>% 
    left_join(env, by = c("Site" = "Site")))

