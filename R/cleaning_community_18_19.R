#################################
##    INCLINE data cleaning    ##
#################################

# Script by Eva Lieungh and Ragnhild Gya

#### Libraries ####
library("tidyverse")

#### Load community data ####
setwd('')
community <- read.csv('INCLINE_community_2018_2019.csv', sep = ';') # 10197 obs. of 180 variables
plot.treatment <- read.csv('Plot_treatment.csv') # treatments per plot

#### Clean and reorganise data ####
community <- community %>%
  filter(Site!="")%>% # remove any lines with no Site
  mutate(Site= substr(Site, 1,3))%>% # Shorten site name to first three letters
  unite(plotID, c(Site,Block,plot), remove = FALSE)%>% # make a new column with unique plot ID from Site,block and plot ID
  select(-Treatment, -X) # remove old treatment column with lots of missing values, anx X only containing NA's
  # 10188 obs of 179 vars
head(community[,1:11]) # look at the head of the first columns (the rest are species columns)

## make a new column with treatment and match with plot
community <- merge(community,plot.treatment,
                    by = 'plotID',
                    all= TRUE)
            # now 10228 obs of 180 vars - not sure what rows were added

## reorder the columns to get the species last
community <- community %>%
  select(1:11, Treatment, moss:comments, everything()) 
head(community[,1:25])


## fill in blanks with 0 for species columns and NA for other variables
community[] <- lapply(community, as.character)
communityOther <- community[,1:25]
communityOther[communityOther==''|is.na(communityOther)] <- 'NA'
communitySpecies <- community[,26:180]
communitySpecies[communitySpecies==''|is.na(communitySpecies)] <- 0
community <- cbind(communityOther,communitySpecies)



## split species records (J/1/D/F) into separate columns - NOT FINISHED CODE
 # community <- community %>%
 # maxchar = max(nchar(as.character(df$y)))  # https://stackoverflow.com/questions/37731324/split-or-separate-uneven-unequal-strings-with-no-delimiter 
 # tidyr::separate(df, y, into = paste0("y", 1:maxchar), sep = 1:(maxchar - 1))
  


## merge uncertain species (i.e. collapse 'cf.' into proposed species)
community <- community %>%
  select(community,contains('_cf'))

t(community) %>%
  data.frame() %>%
  group_by(., id = gsub('\\..*', '', rownames(.))) %>%
  summarise_all(sum) %>%
  data.frame() %>%
  column_to_rownames(var = 'id') %>%
  t()



## save output
write.csv(community,'community_all_cleaned.csv')

#prefixes = unique(substring(names(community2[,28:164]),1,7))# picks out unique species names using the first 7 characters of the column names (i.e. skipping _cf extensions)
#merged <- sapply(prefixes, function(x)rowSums(community2[,startsWith(colnames(community2[,28:164]),prefixes)],x,na.rm = TRUE)) # nope, får det ikke til å funke. Eksempel hentet fra: https://stackoverflow.com/questions/49859020/sum-columns-with-similar-names-in-r 


#### Filter out cover data ####
cover <- community %>%
  filter(Measure == "cover") # keep just the cover data, 232 rows

cover <- cover %>% # not exactly sure what this chunk of code does, copied from Cleaning2 script
  select(-subPlot, -Measure, -recorder, -Nid.seedling, -lichen, -litter, -soil, -rock) %>% 
  gather(species, cover, Ach_mil:Vio_riv)%>%
  mutate(cover = as.numeric(cover))%>%
  filter(!is.na(cover))%>%  #Takes out the species that are not present in the dataset
  mutate(species=gsub("\\.", "_", species))%>%
  mutate(Site = as.character(Site, levels = c("Ulv", "Lav", "Gud", "Skj")))

cover <- cover %>% # find the mean cover by plot (or by site)
  group_by(Site, species)%>%
  mutate(mean_cover=mean(cover, na.rm=TRUE))%>% #If you want the turf data use mutate, if you want the site data use summarise
  ungroup()%>%
  mutate(Site = factor(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs")))%>%
  group_by(turfID)%>%
  mutate(sum_cover= sum(cover))%>%
  mutate(cover_species=cover/sum_cover*100) %>% 
  filter(! turfID == "Lav1XC") #Only has one species in it



#### Filter out community/subplot data ####
community <- community %>%
  filter(Measure == "subPlot") # keep just the subPlot data


## transform data from broad to long format
#community.long <- community %>%
#  gather(plotID, year)


