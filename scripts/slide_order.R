library(tidyverse)

setwd('~/Documents/SoS_GB_EcoRegion_Species_Talk/scripts')
spp <- read.csv('../data/slide_order.csv') %>% 
  mutate(Genus = na_if(Genus, ""), 
         Level = case_when(
           Taxon_treated == Family ~ 'Family',
           Taxon_treated == Genus ~ 'Genus',
           !is.na(Genus) ~ 'Species',
           is.na(Genus) ~ 'Infrafamily'
         ))


spp %>% 
  filter(Level != 'Species') %>% 
  write.csv(., '../data/slides_for_talk.csv')

