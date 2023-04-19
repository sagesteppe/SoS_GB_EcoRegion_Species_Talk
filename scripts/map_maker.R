library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(ggthemes)

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
  st_transform(4326) %>% 
  st_bbox() 

gb_v_bb <- gb %>% 
  st_transform(5070) %>% 
  st_buffer(5000) %>% 
  st_transform(4326) %>% 
  vect() %>% 
  ext()

# import state boundaries here
st <- states() %>% 
  filter(STUSPS %in% c('NV', 'CA', 'ID', 'UT', 'AZ', 'OR')) %>% 
  select(STUSPS) %>% 
  st_transform(4326)

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

## import occurrences here

occ_data <- st_read(
  '/media/sagesteppe/ExternalHD/SoS_GB/scouting/data/SDM-occ/SDM-occ.shp', 
  quiet = T) %>% 
  st_transform(4326)


#' plot many same sdm maps
#' 
#' @param x a raster stack of species to plot
#' @param y an sf tibble of the same species occurrence data to plot
#' @param path_dir directory to save the outfiles

map_maker_fn <- function(x, y, path_dir, name){
  
  
  spp <- gsub('_', ' ', name)
  occurrence <- filter(y, species == spp)
  
  pred <- as.data.frame(x, xy= T)
  pred <- pred %>% 
    rename(species = 3) %>% 
    mutate(species = jitter(species))
  
  ggplot() +
    geom_tile(data = hillshade, aes(x = x, y = y, fill = lyr1))  +
    scale_fill_gradient(low = "grey50", high = "grey100") +
    guides(fill = 'none') +
    ggnewscale::new_scale_fill() + 
    
    geom_tile(data = dem, aes(x = x, y = y, fill = elevation_1KMmd_GMTEDmd)) + 
    scale_fill_viridis_c(direction = -1, alpha = 0.3) + 
    guides(fill = 'none') +
    ggnewscale::new_scale_fill() + 
    
    geom_tile(data = pred, aes(x = x, y = y, fill = species)) + 
    scale_fill_distiller('Probability of suitable habitat      ', 
                         palette = "RdPu", direction = 1, 
                         breaks=c(1, 2.5, 4), limits = c(0.9,4.1),
                         labels=c("Mild", "Medium", "High")) + 
    
    geom_sf(data = st, fill = NA, lwd = 2) + 
    geom_sf(data = occurrence, shape = 18, color = 'black') + 
    
    coord_sf(xlim = c(gb_bb[[1]], gb_bb[[3]]), 
             ylim = c(gb_bb[[2]], gb_bb[[4]]), expand = F) + 
    theme_void() + 
    
    theme(legend.position = 'bottom', 
          legend.box.background = element_rect(color="black", linewidth = 1),
          legend.box.margin = margin(2, 60, 2, 60), 
    )
  
  ggsave(filename = file.path(path_dir, spp), plot = last_plot(), 
         width = 480, height = 480, units = "px",
         dpi = 72, device = 'png', bg = 'white')
  
}


for (i in 1:(dim(preds)[3])){
  
  map_maker_fn(x = preds[[i]], y = occ_data, name = names(preds[[i]]), path_dir = '../figures/maps')
  
}





