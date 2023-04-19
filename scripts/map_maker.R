library(tidyverse)
library(sf)
library(terra)
library(tigris)

setwd('~/Documents/SoS_GB_EcoRegion_Species_Talk/scripts')
# import cartographic dataset here
list.files('../data/spatial')


# ecoregions
gb <- st_read('../data/spatial/NA_CEC_Eco_Level3/NA_CEC_Eco_Level3.shp') %>% 
  filter(NA_L3CODE %in% c('10.1.5', '10.1.3', '10.1.8')) %>% 
  st_union() %>% 
  st_cast(., "POLYGON") %>% 
  st_as_sf() %>% 
  rename(geometry = x) %>% 
  mutate(Area = 'Great Basin') %>% 
  select(Area, geometry) %>% 
  st_transform(4326)

gb_bb <- gb %>% 
  st_transform(5070) %>% 
  st_buffer(5000) %>% 
  st_transform(4326) %>% 
  st_bbox() 

gb_v_bb <- gb %>% 
  st_transform(5070) %>% 
  st_buffer(5000) %>% 
  st_transform(4326) %>% 
  vect() %>% 
  ext()

project(gb_v_bb, to = 'epsg:5070') 

# import state boundaries here
st <- states() %>% 
  filter(STUSPS %in% c('NV', 'CA', 'ID', 'UT', 'AZ', 'OR')) %>% 
  select(STUSPS) %>% 
  st_transform(4326)

st <- st_crop(st, gb_bb)

# import tree cover for forest cover

green1 <- crop(rast('../data/spatial/consensus_full_class_1.tif'), gb_v_bb)
green2 <- crop(rast('../data/spatial/consensus_full_class_3.tif'), gb_v_bb)
green <- sum(green1, green2)

rm(green1, green2)

# import mountains to create hillshade
hills <- crop(rast('../data/spatial/elevation_1KMmd_GMTEDmd.tif'), gb_v_bb)

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
preds <- project(preds, crs(green))
preds <- crop(preds, gb_v_bb)

rm(p2preds, files, omernik)

p37 <- as.data.frame(preds[[37]], xy= T)
p37 <- p37 %>% 
  rename(species = 3) %>% 
  mutate(species = jitter(species))


library(ggthemes)

ggplot() +
  geom_tile(data = hillshade, aes(x = x, y = y, fill = lyr1))  +
  scale_fill_gradient(low = "grey50", high = "grey100") +
  guides(fill = 'none') +
  ggnewscale::new_scale_fill() + 
  
  geom_tile(data = dem, aes(x = x, y = y, fill = elevation_1KMmd_GMTEDmd)) + 
  scale_fill_viridis_c(direction = -1, alpha = 0.3) + 
  guides(fill = 'none') +
  ggnewscale::new_scale_fill() + 
  
  geom_raster(data = p37, aes(x = x, y = y, fill = species)) + 
  scale_fill_distiller('Probability of Suitable Habitat"', 
                       palette = "RdPu", direction = 1) + 
  
  geom_sf(data = st, fill = NA) + 
  
  theme_void() + 
  theme(legend.position = 'bottom')
