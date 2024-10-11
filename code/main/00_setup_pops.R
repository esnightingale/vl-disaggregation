################################################################################
# Read India/Nepal/Bangladesh 100m population rasters, merge and crop to BH
# region with buffer to cover outer mesh region of spatial model
################################################################################

library(tidyverse)
library(sf)
library(raster)
library(gdalUtils)

################################################################################

boundary <- readRDS(here::here("data","geography","bihar_block.rds")) %>% 
  sf::st_set_crs(4326) %>% 
  sf::st_union() 

# Define a large buffer so that have values for outer mesh boundary later
buff <- boundary %>% 
  sf::st_transform(7759) %>% 
  sf::st_buffer(dist = 1e5) %>% 
  sf::st_transform(4326) %>% 
  sf::as_Spatial()

plot(buff)

# Crop to extent of boundary
bb <- sf::st_bbox(buff)
e <- raster::extent(c(bb$xmin, bb$xmax, bb$ymin, bb$ymax))

# ---------------------------------------------------------------------------- #

# Nepal
popNP <- raster::raster(here::here("data","covariates","population","npl_ppp_2018_UNadj.tif"))

# Bangladesh
popBD <- raster::raster(here::here("data","covariates","population","bgd_ppp_2018_UNadj.tif"))

# Read temporary full India raster as too large to load fully
gdalbuildvrt(gdalfile = here::here("data","covariates","population","ind_ppp_2018_UNadj.tif"), 
             output.vrt = "popBH.vrt", 
             te = st_bbox(buff))

popBH <- raster::raster("popBH.vrt")

pop <- popBH %>%
  raster::merge(popNP) %>% 
  raster::merge(popBD) %>% 
  raster::crop(e) %>% 
  raster::mask(buff)

plot(pop)

pop_spdf <- as(pop, "SpatialPixelsDataFrame")
pop_df <- as.data.frame(pop_spdf)

ggplot() +
  geom_raster(data = pop_df, aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(trans = "sqrt") + 
  labs(fill = "Population") +
  coord_quickmap()

ggsave(here::here("figures","pop_villages_100m.png"), height = 6, width = 8)

raster::writeRaster(pop, filename = here::here("data","covariates","pop_100m.tif"), format = "GTiff", overwrite = TRUE)
