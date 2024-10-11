################################################################################
# Extract population counts from Worldpop raster and aggregate to village and 
# block shapefiles.
################################################################################

library(tidyverse)
library(sf)
library(rmapshaper)

# Local data folder
datadir <- "C:/Users/phpuenig/Documents/VL/Data"

# ---------------------------------------------------------------------------- #
# Bihar population raster (cropped from WorldPop India 100m)

popBH <- raster::raster(here::here("data","covariates","pop_100m.tif"))

# ---------------------------------------------------------------------------- #
# Read in village shape file

v_shp <- sf::read_sf(dsn = here::here(datadir,"Shapefiles","official_bihar_india_village_boundary_shapefile"),
                 layer = "Bihar_Village_Boundary") %>%
  dplyr::select(c("OBJECTID","VILL_CODE","V_CODE2011","V_NAME",
                  "BLK_NAME","BLK_C_2011",
                  "GPCODE2011", "GPNAME_1","DIST_NAME",
                  "ST_NAME","ST_CODE_1",
                  "MAP_STATUS", "AREA")) %>%
  dplyr::mutate(DIST_NAME = as.character(DIST_NAME),
                DIST_NAME = stringr::str_replace(DIST_NAME, '\\*', 'AN')) %>%
  sf::st_transform(crs = crs(popBH)) 

v_shp %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(DIST_NAME, BLK_NAME) %>% 
  unique() %>% 
  mutate(across(everything(), toupper)) -> names

# ---------------------------------------------------------------------------- #
# Simplify village polygons

v_shp_simp <- ms_simplify(v_shp, 
                          keep = 0.001,
                          keep_shapes = FALSE)
plot_map(v_shp_simp, strokecolor = '#097FB3',
         fillcolor = '#AED3E4')

# ---------------------------------------------------------------------------- #
# Aggregate pop raster to villages

v_shp$pop_raster_agg <- exactextractr::exact_extract(x = popBH, y = v_shp, fun = "sum")

summary(v_shp$pop_raster_agg)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0     595.1    1398.8    2715.7    3077.1 1741400.8 

ggplot(v_shp, aes(geometry = geometry, fill = pop_raster_agg)) +
  geom_sf(col = NA) +
  scale_fill_viridis_c(trans = "sqrt") +
  labs(fill = "Population") -> v_pops
v_pops
ggsave(here::here("figures","pop_villages.png"), height = 6, width = 8)

# ---------------------------------------------------------------------------- #
# Aggregate to block

v_shp %>% 
  group_by(BLK_C_2011) %>% 
  summarise(pop_raster_agg = sum(pop_raster_agg)) %>% 
  ungroup() %>% 
  nngeo::st_remove_holes() -> b_shp

summary(b_shp$pop_raster_agg)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 10456  139188  206298  228228  284081 2076411 

ggplot(b_shp, aes(geometry = geometry, fill = pop_raster_agg)) +
  geom_sf(col = NA) +
  scale_fill_viridis_c(trans = "sqrt") +
  labs(fill = "Population") -> b_pops
b_pops
ggsave(here::here("figures","pop_blocks_vilagg.png"), height = 6, width = 8)

# ---------------------------------------------------------------------------- #
# Read in block shape file

# mchdnms <- read_csv(file.path(datadir, "MatchedBlockNames.csv")) %>% 
  # dplyr::select(district, admin3, kamisdist, kamisblock)

# b_shp <- readRDS(here::here("data","geography","bihar_block.rds")) %>% 
#   sf::st_set_crs(4326)  %>%
#   sf::st_transform(crs = crs(popBH)) 
#   # Add matching names
#   # dplyr::left_join(mchdnms) %>% 
#   # dplyr::mutate(kamisdist = ifelse(is.na(kamisdist), kamis_master_dist, kamisdist),
#   #               kamisblock = ifelse(is.na(kamisblock), kamis_master_block, kamisblock)) %>% 
#   # dplyr::select(-district, -admin3, -kamis_master_dist, -kamis_master_block)
# 
# # Aggregate raster to blocks
# b_shp$pop_raster_agg <- exactextractr::exact_extract(x = popBH, y = b_shp, fun = "sum")
# 
# hist(b_shp$pop_raster_agg)
# summary(b_shp$pop_raster_agg)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# # 20628  141291  206402  228006  285547 2161809 
# 
# ggplot(b_shp, aes(geometry = geometry, fill = pop_raster_agg)) +
#   geom_sf(col = NA) +
#   scale_fill_viridis_c(trans = "sqrt") +
#   labs(fill = "Population") -> b_pops
# b_pops
# ggsave(here::here("figures","pop_blocks.png"), height = 6, width = 8)

################################################################################

saveRDS(popBH, here::here("data","geography","pop_raster.rds"))
saveRDS(v_shp, here::here("data","geography","v_shp_wpops.rds"))
saveRDS(b_shp, here::here("data","geography","b_shp_wpops.rds"))

################################################################################
################################################################################
