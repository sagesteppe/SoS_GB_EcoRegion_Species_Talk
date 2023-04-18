library(tidyverse)
library(sf)
library(terra)
library(tigris)

setwd('~/Documents/SoS_GB_EcoRegion_Species_Talk/scripts')
# import cartographic dataset here
list.files('../data/spatial')


# import state boundaries here
st <- states() %>% 
  filter(STUSPS %in% c('NV', 'CA', 'ID', 'UT', 'AZ', 'OR')) %>% 
  select(STUSPS)


# import tree cover for forest cover
st_v <- vect(st)
green1 <- crop(rast('../data/spatial/consensus_full_class_1.tif'), st_v)
green2 <- crop(rast('../data/spatial/consensus_full_class_3.tif'), st_v)
green <- sum(green1, green2)

rm(green1, green2)

# import mountains to create hillshade
hills <- crop(rast('../data/spatial/elevation_1KMmd_GMTEDmd.tif'), st_v)

slope <- terrain(hills, "slope", unit="radians")
aspect <- terrain(hills, "aspect", unit="radians")
hill <- shade(slope, aspect, 30, 315)

dem <- as.data.frame(hills, xy = T)
hillshade <- as.data.frame(hill, xy = T)

rm(st_v, slope, aspect, hill, hills)

# import sdm predictions here
p2preds <- '/media/sagesteppe/ExternalHD/SoS_GB/scouting/results/preds_cat'
files <- list.files(path = p2preds, pattern = 'tif$')
preds <- rast(file.path(p2preds, files))
names(preds) <- gsub('_2023.*$', "", names(preds))
preds <- aggregate(preds, 5)

rm(p2preds, files)

p37 <- as.data.frame(preds[[37]], xy= T)

p37 <- p37 %>% 
  mutate(Lomatium_triternatum = jitter(Lomatium_triternatum))


ggplot() +
  geom_tile(data = hillshade, aes(x = x, y = y, fill = lyr1))  +
  scale_fill_gradient(low = "grey50", high = "grey100") +
  
  theme_bw()

