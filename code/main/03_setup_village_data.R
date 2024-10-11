library(tidyverse)
library(sf)
library(INLA)
library(inlabru)
library(raster)
library(rgeos)
library(patchwork)
library(mgcv)
library(mgcViz)

theme_set(theme_minimal())

# inla.setOption(inla.mode="experimental")

# Local data folder
datadir <- "C:/Users/phpuenig/Documents/VL/Data"

# ---------------------------------------------------------------------------- #
# Read in data 

# Block polygons with 2018 case counts + populations
shapes <- readRDS(here::here("data","aggregate","b_shp_vl.rds")) %>% 
  mutate(dist_blk = paste(kamis_master_dist, kamis_master_block, sep = ":"),
         block_pop = pop_raster_agg) %>% 
  st_transform(4326) %>% 
  dplyr::select(dist_blk, kamis_master_dist, kamis_master_block, block_pop, X2018_inc_blk, geometry)
shapes$dist_blk_id <- as.numeric(as.factor(shapes$dist_blk))

# Bihar state boundary
boundary_sf <- st_union(shapes)

# Covariates and population raster for Bihar region
covariate_stack <- readRDS(here::here("data","covariates","analysis","cov_stack_1km_std.rds"))
pop_rast <- covariate_stack$pop
covariate_stack <- covariate_stack[[-1]]

# Village incidence data
v_sf <- readRDS(here::here(datadir, "KAMIS/Clean/village", "v_cent_vl.rds")) %>% 
  st_transform(4326) %>% 
  mutate(affect_3yr = (X2015 + X2016 + X2017) > 0) 
# dplyr::select(OBJECTID, pop_care_estimate, pop_raster_agg, X2015:X2017, affect_3yr, X2018, X2018_inc, geometry) 

# Village polygons
v_shp_sf <- readRDS(here::here(datadir, "KAMIS/Clean/village", "v_shp_vl.rds")) %>% 
  st_transform(4326) %>% 
  dplyr::select(OBJECTID, geometry)

# ---------------------------------------------------------------------------- #
# Combine village polygon data with block incidence

# Join block data from shapefile with village point data according to where 
# centroids fall in the polygons. Once merged, drop geometry and redefine from village coords.
v_blk_sf <- shapes %>%
  st_join(v_sf, join = st_intersects) %>% 
  st_drop_geometry() 

# Block polygons may overlap so one point may fall within more than one polygon and
# this ends up duplicating some villages
dupids <- v_blk_sf$OBJECTID[duplicated(v_blk_sf$OBJECTID)]
dupids
# 399  2739  8768  9234 14781 19838 20529 20530

# View(v_blk_sf[v_blk_sf$OBJECTID %in% dupids,])

# As there are only a few, manually remove duplicates based on the block specified in 
# the village shapefile
rmv <- which((v_blk_sf$OBJECTID == 399 & v_blk_sf$kamis_master_block == "KURSAKANTA")|
               (v_blk_sf$OBJECTID == 2739 & v_blk_sf$kamis_master_block == "NABINAGAR")|
               (v_blk_sf$OBJECTID == 8768 & v_blk_sf$kamis_master_block == "BUXAR")|
               (v_blk_sf$OBJECTID == 9234 & v_blk_sf$kamis_master_block == "CHAKKI")|
               (v_blk_sf$OBJECTID == 14781 & v_blk_sf$kamis_master_block == "GOPALGANJ SADAR")|
               (v_blk_sf$OBJECTID == 19838 & v_blk_sf$kamis_master_block == "BARSOI")|
               (v_blk_sf$OBJECTID %in% c(20529,20530) & v_blk_sf$kamis_master_block == "BAISA"))

v_blk_sf <- v_blk_sf[-rmv,]

# Merge this dataset with village shapefiles by objectid
v_blk_sf <- v_shp_sf %>%
  full_join(v_blk_sf)

# ---------------------------------------------------------------------------- #
# Check by plotting block and village incidence

ggplot(v_blk_sf, aes(fill = X2018_inc*1000)) +
  geom_sf(col = NA) +
  scale_fill_viridis_c(trans = "sqrt") -> p1

ggplot(v_blk_sf, aes(fill = X2018_inc_blk*1e4)) +
  geom_sf(col = NA) +
  scale_fill_viridis_c(trans = "sqrt") -> p2

p <- p1 + p2
ggsave(here::here("figures","village_block_incidence.png"), p, height = 6, width = 20)

# ---------------------------------------------------------------------------- #
# Add covariates averaged over polygons

covs <- exactextractr::exact_extract(covariate_stack, v_blk_sf, fun = "mean")
names(covs) <- gsub("mean.","", names(covs))

v_blk_sf <- bind_cols(v_blk_sf, covs) 

# Redefine geometry to centroid for analysis
v_blk_sf_cent <- v_blk_sf %>% 
  st_transform(7759) %>% 
  st_centroid() %>% 
  st_transform(4326)

## FINAL ANALYSIS DATASET ##
saveRDS(v_blk_sf, here::here("data","analysis","village_shp_wblk_wcovs.rds"))
saveRDS(v_blk_sf_cent, here::here("data","analysis","village_cent_wblk_wcovs.rds"))

################################################################################