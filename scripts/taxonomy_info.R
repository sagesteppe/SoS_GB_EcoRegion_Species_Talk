library(taxizedb)
library(taxize)
library(tidyverse)

setwd('~/Documents/SoS_GB_EcoRegion_Species_Talk/scripts')
spp <- read.csv('../data/species_data.csv')

spp_v <- sort(pull(spp, Species))

write.csv(data.frame(Species = spp_v), '../data/species-Authorities.csv', 
          row.names = F) # manually check kew plants of the world

spp_v <- spp_v[!spp_v %in% c('Poa secunda', 'Peritoma lutea')]

sp_v_syn <- synonyms(spp_v, db = 'itis')
sp_v_syn_full <- purrr::compact(sp_v_syn) 
sp_v_syn_full <- data.table::rbindlist(sp_v_syn_full, fill = T, idcol = T)

syn_names <- c('Oryzopsis hymenoides', 'Stipa hymenoides', 'Eriocoma hymenoides',  # last is accepted
               'Stipa lemmonii', 'Stipa columbiana', 'Eriocoma lemmonii',  # last is accepted
               'Stipa lettermannii', 'Eriocoma lettermanii',  # last is accepted
               'Stipa occidentalis', 'Eriocoma occidentalis',  # last is accepted
               'Stipa speciosa', 'Pappostipa speciosa', # last is accepted
               'Stipa nevadensis', 'Eriocoma nevadensis', # last is accepted
               'Stipa thurberiana', 'Eriocoma thurberiana',  # last is accepted
               'Stipa webberi', 'Oryzopsis webberi', 'Eriocoma webberi',  # last is accepted
               'Machaeranthera canescens', 
               'Pleuraphis jamesii',
               'Sitanion hystrix', 'Elymus sitanion', 
               'Stipa comata', 
               'Koeleria nitida', 'Koeleria gracilis',
               'Elymus cinereus', 
               'Microseris troximoides', 
               'Agropyron spicatum', 'Elymus spicatus' 
)

sp_v_syn_full <- filter(sp_v_syn_full, syn_name %in% syn_names) %>% 
  select(Species = '.id', syn_name, syn_author) %>% 
  mutate(Species = str_replace(Species, 'Pleuraphis', 'Hilaria'))

relevant <- data.frame(
  Species = c('Peritoma lutea', 'Achnatherum speciosum', 'Poa secunda', 'Heliomeris multiflora'), 
  syn_name = c('Cleome lutea', 'Pappostipa speciosa', 'Poa sandbergii', 'Viguiera multiflora'), 
  syn_author = c('Hook.', '(Trin. & Rupr.) Romasch', 'Vasey', '(Nutt.) S.F. Blake')
)

with_synonyms <- bind_rows(sp_v_syn_full, relevant) %>% 
  mutate(New_Accepted = if_else(str_detect(syn_name, 'Eriocoma|Pappostipa'), 'New', 'Old'))
            
spp_v <- spp_v[spp_v != 'Pleuraphis jamesii']
spp_v <- spp_v[!spp_v %in% with_synonyms$Species]

with_synonyms <- bind_rows(with_synonyms, data.frame(Species = spp_v))

synonyms <- arrange(with_synonyms, Species)
write.csv(synonyms, '../data/species_data-synoyms.csv', row.names = T)

rm(relevant, sp_v_syn, sp_v_syn_full, syn_names, spp, synonyms, with_synonyms, spp_v)
