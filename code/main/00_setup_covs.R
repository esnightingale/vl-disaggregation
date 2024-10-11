################################################################################
# Read India/Nepal/Bangladesh 100m covariate raster, merge and crop to BH
# region with buffer to cover outer mesh region of spatial model
################################################################################

library(tidyverse)
library(sf)
library(raster)
library(gdalUtils)
library(patchwork)
library(malariaAtlas)
library(gdistance)
library(abind)

theme_set(theme_minimal())

################################################################################

boundary <- readRDS(here::here("data","analysis","boundary.rds")) %>% 
  sf::st_transform(4326) 

# Define a large buffer so that have values for outer mesh boundary later
buff <- boundary %>% 
  sf::st_transform(7759) %>% 
  sf::st_buffer(dist = 1e5) %>% 
  sf::st_transform(4326) %>% 
  sf::as_Spatial()

plot(buff)
lines(as_Spatial(boundary))

# Create polygon from extent of buffer
bb <- sf::st_bbox(buff)
e <- raster::extent(c(bb$xmin, bb$xmax, bb$ymin, bb$ymax))
e.sp <- as(e, 'SpatialPolygons')
crs(e.sp) <- st_crs(4326)$wkt
# shapefile(e.sp, here::here("data","analysis","extent.shp"))

# ---------------------------------------------------------------------------- #
# MAP city access data 

ind.shp <- malariaAtlas::getShp(ISO = "IND", admin_level = "admin1")
bh.shp <- ind.shp[ind.shp@data$name_1 == "Bihar",] 

cityaccess <- malariaAtlas::getRaster(
  surface = "A global map of travel time to cities to assess inequalities in accessibility in 2015",
  extent = bbox(bh.shp))

cityaccess <- cityaccess %>% 
  raster::crop(e) %>% 
  raster::mask(buff)

city_bh <- mask(cityaccess, as_Spatial(boundary))
cov_spdf <- as(city_bh, "SpatialPixelsDataFrame")
cov_df <- as.data.frame(cov_spdf)
names(cov_df)[1] <- "layer"

ggplot() +
  geom_raster(data = cov_df, aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(trans = "sqrt") +
  labs(fill = "Travel time", x = "", y = "") +
  coord_quickmap() -> p
ggsave(here::here("figures","covariates","city_traveltime.png"), height = 6, width = 8)

raster::writeRaster(cityaccess, filename = here::here("data","covariates","analysis","cityaccess.tif"), format = "GTiff", overwrite = TRUE)

# ---------------------------------------------------------------------------- #
# Worldpop data 
# cov <- "pop"
# filename <- "_ppp_2018_UNadj"

covs <- c("pop","elev","dist_majroad","dist_water","built_ext")
files <- c("_ppp_2018_UNadj","_srtm_topo_100m","_osm_dst_road_100m_2016","_esaccilc_dst_water_100m_2000_2012","_bsgme_v0a_100m_2018")

# cov <- "built_ext"
# filename <- "_bsgme_v0a_100m_2018"

setup_worldpop_cov <- function(cov, filename){

  # Nepal
  NP <- raster::raster(here::here("data","covariates",paste0("npl",filename,".tif")))
  
  # Bangladesh
  BD <- raster::raster(here::here("data","covariates",paste0("bgd",filename,".tif")))
  
  # Temporarily full India raster as too large to load fully
  # Save a subset cropped to the specified extent
  gdalbuildvrt(gdalfile = here::here("data","covariates",paste0("ind",filename,".tif")),
               output.vrt = paste0(cov,"BH.vrt"),
               te = e,
               overwrite = TRUE)

  BH <- raster::raster(paste0(cov,"BH.vrt"))
  
  # BH <- raster::raster(here::here("data","covariates",paste0("ind",filename,".tif"))) %>% 
  #   raster::crop(e)
  
  merged <- BH %>%
    raster::merge(NP) %>% 
    raster::merge(BD) %>% 
    raster::crop(e) %>% 
    raster::mask(buff)
  
  plot(merged)
  
  merged_bh <- mask(merged, as_Spatial(boundary))
  cov_spdf <- as(merged_bh, "SpatialPixelsDataFrame")
  cov_df <- as.data.frame(cov_spdf)
  
  ggplot() +
    geom_raster(data = cov_df, aes(x = x, y = y, fill = layer)) +
    scale_fill_viridis_c(trans = "sqrt") +
    labs(fill = cov, x = "", y = "") +
    coord_quickmap() -> p
  
  ggsave(here::here("figures","covariates",paste0(cov,"_100m.png")),p, height = 6, width = 8)
  
  raster::writeRaster(merged, filename = here::here("data","covariates","analysis",paste0(cov, "_100m.tif")), format = "GTiff", overwrite = TRUE)

}

purrr::map2(covs, files, setup_worldpop_cov)

# ---------------------------------------------------------------------------- #
# MODIS data

# Land surface temperature
lst.files <- list.files(here::here("data","covariates","MODIS","LST"), pattern = "MOD11A2.061_LST")

make_rast <- function(f){
  r <- raster::raster(here::here("data","covariates","MODIS","LST",f)) %>% 
    raster::crop(e) %>% 
    raster::mask(buff)
  return(r)
}

lst1 <- make_rast(lst.files[1])
# MOD11A2.061_LST_Day_1km_doy2017361_aid0001
# Min.                                         13921
# 1st Qu.                                      14688
# Median                                       14816
# 3rd Qu.                                      14897
# Max.                                         15167
# NA's                                        254156

lst <- lapply(lst.files, make_rast) %>% 
  raster::brick() 

# Find NA values in rasters
fillvalue <- lst@file@nodatavalue
lst[lst == fillvalue] <- NA

# Transform according to specified scaling factor and convert from kelvin to celsius
lst <- lst*0.02 - 273.15

lst_mean <- raster::calc(lst, fun = mean, na.rm = T) 
lst_sd <- raster::calc(lst, fun = sd, na.rm = T) 

# Normalised difference vegetation index
ndvi.files <- list.files(here::here("data","covariates","MODIS","NDVI"), pattern = "MOD13A3.061__1_km_monthly_NDVI")

ndvi <- lapply(ndvi.files,
              function(f) raster::raster(here::here("data","covariates","MODIS","NDVI",f))) %>% 
  raster::brick() %>% 
  raster::crop(e) %>% 
  raster::mask(buff)

# Find NA values in rasters
fillvalue <- ndvi@file@nodatavalue
ndvi[ndvi == fillvalue] <- NA

# Transform according to specified scaling factor
ndvi <- ndvi*0.0001

ndvi_mean <- raster::calc(ndvi, fun = mean, na.rm = T) 
ndvi_sd <- raster::calc(ndvi, fun = sd, na.rm = T) 

modis_rasters <- list(lst_mean = lst_mean, lst_sd = lst_sd, 
                      ndvi_mean = ndvi_mean, ndvi_sd = ndvi_sd)

stk <- raster::stack(modis_rasters)
plot(stk)

save_rast <- function(x, nm){
  raster::writeRaster(x, 
                      filename = here::here("data","covariates","analysis",paste0(nm, "_modis.tif")),
                      format = "GTiff", 
                      overwrite = TRUE)
}
purrr::map2(modis_rasters, names(modis_rasters), save_rast)

# ---------------------------------------------------------------------------- #

plot_cov <- function(cov, nm){
  
  cov <- raster::mask(cov, as_Spatial(boundary))
  
  cov_spdf <- as(cov, "SpatialPixelsDataFrame")
  cov_df <- as.data.frame(cov_spdf)
  
  ggplot() +
    geom_raster(data = cov_df, aes(x = x, y = y, fill = layer)) +
    # geom_polygon(data = as_Spatial(boundary), aes(long, lat), fill = NA, col = "darkgrey") +
    scale_fill_viridis_c(trans = "identity", na.value = "white") + 
    labs(fill = nm, x = "", y = "") +
    coord_quickmap() -> p
  # p
  
  ggsave(here::here("figures","covariates",paste0(nm,"_modis.png")), p, height = 6, width = 8, dpi = 250)
  
}
purrr::map2(modis_rasters, names(modis_rasters), plot_cov)


# ---------------------------------------------------------------------------- #
# Plot covariate values for each village

# v_shp_vl <- readRDS(here::here("data","analysis","v_shp_vl.rds")) %>% 
#   st_transform(4326)
# 
# v_shp_vl %>% 
#   filter(X2018_inc > 0) %>%
#   ggplot(aes(x = elev, y = X2018_inc*1000)) +
#   geom_point(alpha = 0.2) +
#   geom_smooth() + 
#   # scale_x_continuous(trans = "log2") +
#   scale_y_continuous(trans = "log10") +
#   labs(x = "elevation", y = "Village incidence per 1000, 2018") -> elev
# 
# v_shp_vl %>% 
#   filter(X2018_inc > 0) %>%
#   ggplot(aes(x = cityaccess, y = X2018_inc*1000)) +
#   geom_point(alpha = 0.2) +
#   geom_smooth() + 
#   # scale_x_continuous(trans = "log2") +
#   scale_y_continuous(trans = "log10") +
#   labs(x = "Travel time to urban area", y = "Village incidence per 1000, 2018") -> traveltime
# 
# v_shp_vl %>% 
#   filter(X2018_inc > 0) %>%
#   ggplot(aes(x = dist_water, y = X2018_inc*1000)) +
#   geom_point(alpha = 0.2) +
#   geom_smooth() + 
#   # scale_x_continuous(trans = "log2") +
#   scale_y_continuous(trans = "log10") +
#   labs(x = "Distance to major waterway", y = "Village incidence per 1000, 2018") -> dist_water
# 
# v_shp_vl %>% 
#   filter(X2018_inc > 0) %>%
#   ggplot(aes(x = lst_mean, y = X2018_inc*1000)) +
#   geom_point(alpha = 0.2) +
#   geom_smooth() + 
#   # scale_x_continuous(trans = "log2") +
#   scale_y_continuous(trans = "log10") +
#   labs(x = "LST (annual mean)", y = "Village incidence per 1000, 2018") -> lst1
# 
# v_shp_vl %>% 
#   filter(X2018_inc > 0) %>%
#   ggplot(aes(x = lst_sd, y = X2018_inc*1000)) +
#   geom_point(alpha = 0.2) +
#   geom_smooth() + 
#   # scale_x_continuous(trans = "log2") +
#   scale_y_continuous(trans = "log10") +
#   labs(x = "LST (annual sd)", y = "Village incidence per 1000, 2018") -> lst2
# 
# elev + dist_road + dist_water
# ggsave(here::here("figures","covs_vs_vil_inc.png"), height = 6, width = 24)
# 
