################################################################################
# Description: Aggregate village data to block
################################################################################

library(sf)
library(raster)
library(tidyverse)
library(rmapshaper)

# Turn off s2 processing to avoid invalid spherical geometry errors
sf::sf_use_s2(FALSE)

theme_set(theme_minimal())

# Fix default ggsave background
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

# Local data folder
datadir <- "C:/Users/phpuenig/Documents/VL/Data"

# Output folder
outdir <- file.path(datadir, "KAMIS/Clean/village")

# Bihar population raster (cropped from WorldPop India 100m)
popBH <- raster::raster(here::here("data","covariates","pop_100m.tif"))

################################################################################

# Load village polygons

v_shp <- sf::read_sf(dsn = here::here(datadir,"Shapefiles","official_bihar_india_village_boundary_shapefile"),
                     layer = "Bihar_Village_Boundary") %>%
  dplyr::mutate(DIST_NAME = as.character(DIST_NAME),
                DIST_NAME = stringr::str_replace(DIST_NAME, '\\*', 'AN')) %>% 
  dplyr::select(OBJECTID, DIST_NAME, BLK_NAME, BLK_C_2011, GPNAME_1, V_CODE2011, V_NAME, geometry) %>% 
  mutate(across(DIST_NAME:V_NAME, toupper)) %>% 
  st_transform(7759) %>% 
  filter(DIST_NAME != "UNIDENTIFIED")

# ---------------------------------------------------------------------------- #
# Define village census code

# Want to aggregate shapes by 2011 census code, to simplify. However, some are missing:
summary(v_shp$V_CODE2011 %in% c(0, NA))
# Mode   FALSE    TRUE 
# logical   43958     835 

# Map these out
pdf(here::here("figures/cleaning", "missing_vcode.pdf"), height = 7, width = 9)
ggplot(v_shp, aes(fill = (V_CODE2011 %in% c(0, NA)))) + 
  geom_sf()
dev.off()

# Where code is missing, generate a new one so shapes are not aggregated as one
sort(unique(as.numeric(v_shp$V_CODE2011)))[1:10]

v_shp$v_code_imp <- as.numeric(v_shp$V_CODE2011)
idx <- which(v_shp$v_code_imp %in% c(0,NA))
v_shp$v_code_imp[idx] <- 1:nrow(v_shp[idx,])

sort(unique(v_shp$v_code_imp))[1:1000]

# No. villages
n_distinct(v_shp$v_code_imp) # 44490

# No. blocks
st_drop_geometry(v_shp) %>% dplyr::select(DIST_NAME, BLK_NAME) %>% unique() %>% nrow() # 541
st_drop_geometry(v_shp) %>% dplyr::select(BLK_C_2011) %>% unique() %>% nrow() # 532

# Look up between village code and block names for later aggregation

# Fix one pesky GP with special characters
v_shp$GPNAME_1[v_shp$DIST_NAME == "MUZAFFARPUR" & 
                 v_shp$BLK_NAME == "SAKRA" & 
                 v_shp$V_NAME == "DONWAN"] <- "RAGHUNATHPUR DONMA"

v_df <- st_drop_geometry(v_shp)

v_shp %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(DIST_NAME, BLK_NAME, v_code_imp) %>% 
  unique() %>% 
  mutate(across(everything(), toupper)) -> blk_lookup

# ---------------------------------------------------------------------------- #
# Define boundary

boundary <- v_shp %>% 
  sf::st_union() %>% 
  sfheaders::sf_remove_holes() %>% 
  rmapshaper::ms_simplify(keep = 0.05,
                          keep_shapes = FALSE)

plot(boundary)

# ---------------------------------------------------------------------------- #
# Village level case data

vl_bh <- read.csv(here::here(datadir,"KAMIS","Raw","village","village level data-Bi+Jh.csv"), header=TRUE) %>% 
  filter(state == "BIHAR") %>% 
  dplyr::select(vil_code, district, block, hsc, village, population, latitude, longitude, X2013:X2018, Total_VL) %>% 
  mutate(dist_blk = paste(district, block, sep = ":"))

summary(is.na(vl_bh$latitude[vl_bh$Total_VL > 0]))
#    Mode   FALSE    TRUE 
# logical    9538     535 

summary(is.na(vl_bh$latitude[vl_bh$X2018 > 0]))
#    Mode   FALSE    TRUE 
# logical    2126      60 

summary(vl_bh$population[vl_bh$X2018 > 0])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   71    1451    2500    3346    4343   19165 

# Edit names to match block shapefile
# vl_bh$district[vl_bh$district == "ARWAL"] <- "JEHANABAD"
# vl_bh$block[vl_bh$block == "KUSHESHWAR ASTHAN (SATIGHAT)"] <- "KUSHESHWAR ASTHAN"
# vl_bh$block[vl_bh$block == "NARKATIA (CHHAURADANO)"] <- "NARKATIA"
# vl_bh$block[vl_bh$block == "PATNA SADAR"] <- "PATNA RURAL"
# vl_bh$block[vl_bh$block == "RAHIKA"] <- "MADHUBANI"
# vl_bh$block[vl_bh$block == "UDAKISHUNGANJ"] <- "UDA KISHAN GANJ"

# Split villages with and without GPS
vl_gps <-  vl_bh %>% 
  filter(!(is.na(longitude)|is.na(latitude) & X2018 > 0)) 

# Pull missing GPS just for 2018 affected villages 
vl_nogps <- vl_bh %>% 
  filter((is.na(longitude)|is.na(latitude)) & X2018 > 0)

# ---------------------------------------------------------------------------- #
# Try to manually match 2018 affected villages missing GPS

vl_bh %>% 
  filter(X2018 > 0 & is.na(longitude)) %>% 
  group_by(dist_blk) %>% 
  summarise(n = sum(X2018),
            n_vil = n_distinct(vil_code)) -> missgps_byblk

# Look for matches in shape data
# v_df %>% 
#   filter(DIST_NAME == "MUZAFFARPUR") %>% View()

# File with matched names
match <- read_csv(here::here("data","village_nogps_namematch.csv"))

# match %>% 
#   left_join(v_shp) %>% View

# Merge to shapefile on matched names, and generate coordinates from shape centroid
vl_nogps %>% 
  left_join(match) %>% 
  left_join(dplyr::select(v_shp, -v_code_imp)) %>% 
  st_sf() %>% 
  st_centroid() %>%
  # transform to lat/long
  st_transform(4326) %>% 
  mutate(longitude = sf::st_coordinates(.)[,1],
         latitude = sf::st_coordinates(.)[,2]) %>% 
  # Finally drop geometry as we're going to redefine this when joined back with GPS villages
  st_drop_geometry() %>% 
  dplyr::select(-BLK_NAME:-V_CODE2011) -> vl_gpsimp

# ---------------------------------------------------------------------------- #
# Rejoin complete GPS with imputed GPS data 

vl_gpsimp %>% 
  bind_rows(vl_gps) -> vl_bh_imp

# Check how many remaining without GPS
summary(is.na(vl_bh_imp$latitude))
#    Mode   FALSE    TRUE 
# logical    9964       5  
sum(vl_bh_imp$X2018[is.na(vl_bh_imp$latitude)])
# 7 cases

# Make into sf object to overlay with polygons
vl_sf <- vl_bh_imp %>%
  # Exclude villages with no coordinates
  filter(!(is.na(longitude)|is.na(latitude))) %>%
  # Create sf object
  st_as_sf(coords = c("longitude","latitude"), remove = FALSE) %>%
  st_set_crs(4326) %>% 
  st_transform(7759)

# ---------------------------------------------------------------------------- #
# Check large location errors

buffer <- boundary %>% 
  sf::st_buffer(1e4)

ggplot() +
  geom_sf(data = boundary) +
  geom_sf(data = buffer, fill = NA) +
  geom_sf(data = vl_sf) +
  theme_minimal()
ggsave(here::here("figures","cleaning","points_10km_outside_state.png"), height = 6, width = 8)

outside <- sapply(st_intersects(vl_sf, buffer),function(x){length(x)==0})
# View(vl_sf[outside,])

# BALIRAJPUR/Babubarhi block/Madhubani district: 
# In data: 26.87, 86.32
# Google maps: 26.46, 86.32
vl_bh_imp$latitude[vl_bh_imp$vil_code == "INDIA\\BIHAR\\MADHUBANI\\BABUBARHI\\BHUPATTI\\BALIRAJPUR"] <- 26.46
# RUPNI/Bahadurganj block/Kishanganj district: 
# In data: 24.24, 87.69
# Google maps: 26.24, 87.69
vl_bh_imp$latitude[vl_bh_imp$vil_code == "INDIA\\BIHAR\\KISHANGANJ\\BAHADURGANJ\\RUPNI\\RUPNI"] <- 26.24
  
vl_sf <- vl_bh_imp %>% 
  filter(!(is.na(longitude)|is.na(latitude))) %>% 
  st_as_sf(coords = c("longitude","latitude"), remove = FALSE) %>%
  st_set_crs(4326) %>% 
  st_transform(7759)

# Check points now corrected
ggplot() +
  geom_sf(data = boundary) +
  geom_sf(data = buffer, fill = NA) +
  geom_sf(data = vl_sf) +
  theme_minimal()

# ---------------------------------------------------------------------------- #
# Marginal errors 

## Points which don't intersect with a village polygon

outside <- sapply(st_intersects(vl_sf, v_shp),function(x){length(x)==0})
summary(outside)
#    Mode   FALSE    TRUE 
# logical    9956       8 

vl_sf[outside,] %>%
  ggplot() +
  geom_sf(data = boundary) +
  geom_sf()

orig <- vl_sf
vl_sf[outside,] <- st_snap(vl_sf[outside,], v_shp, tolerance = units::set_units(500, m))

outside2 <- sapply(st_intersects(vl_sf, v_shp),function(x){length(x)==0})
summary(outside2)
# Mode   FALSE    TRUE 
# logical    9962       2

vl_sf[outside2,] %>%
  ggplot() +
  geom_sf(data = boundary) +
  geom_sf()

vl_sf[outside2,] <- st_snap(vl_sf[outside2,], v_shp, tolerance = units::set_units(800, m))

outside3 <- sapply(st_intersects(vl_sf, v_shp),function(x){length(x)==0})
summary(outside3)
# Mode   FALSE    TRUE 
# logical    9963       1 

vl_sf[outside3,] <- st_snap(vl_sf[outside3,], v_shp, tolerance = units::set_units(1500, m))

outside4 <- sapply(st_intersects(vl_sf, v_shp),function(x){length(x)==0})
summary(outside4)
# Mode   FALSE 
# logical    9964 

# Check how far points have been adjusted
vl_sf[outside,] %>%
  ggplot() +
  geom_sf(data = boundary) +
  geom_sf(data = orig[outside, ], col = "red") +
  geom_sf(col = "green")
ggsave(here::here("figures","cleaning","snapped_points_vshp.png"), height = 6, width = 8)

# ---------------------------------------------------------------------------- #
# Overlay and aggregate to village

# Join shapes with point data
v_shp_vl <- v_shp %>% 
  st_join(vl_sf, join = st_intersects)

# Aggregate polygons by village names and annual VL incidence across points within each resulting polygon - retain polygon geometry
v_shp_vl <- v_shp_vl %>% 
  group_by(OBJECTID, DIST_NAME, BLK_NAME, GPNAME_1, V_NAME, v_code_imp) %>% 
  summarise(across(X2013:X2018, sum, na.rm = TRUE),
            pop_care_estimate = sum(population, na.rm = TRUE)) %>% 
  ungroup() 

v_shp_vl %>% 
  ggplot() +
  geom_sf(data = boundary, fill = "grey") +
  geom_sf(aes(fill = X2018), col = NA) +
  scale_fill_viridis_c(na.value = "grey")

 # ---------------------------------------------------------------------------- #
# Extract population count per polygon from raster

v_shp_vl$pop_raster_agg <- exactextractr::exact_extract(x = popBH, y = v_shp_vl, fun = "sum")
v_shp_vl$X2018_inc <- v_shp_vl$X2018/v_shp_vl$pop_raster_agg

ggplot(v_shp_vl, aes(geometry = geometry, fill = pop_raster_agg)) +
  geom_sf(data = boundary, fill = "grey") +
  geom_sf(col = NA) +
  scale_fill_viridis_c(trans = "sqrt") +
  labs(fill = "Population") -> v_pops
v_pops
ggsave(here::here("figures","pop_villages.png"), height = 6, width = 8)

ggplot(v_shp_vl, aes(geometry = geometry, fill = X2018*1000/pop_raster_agg)) +
  geom_sf(data = boundary, fill = "grey") +
  geom_sf(col = NA) +
  scale_fill_viridis_c(trans = "sqrt") +
  labs(fill = "Incidence/1,000") -> v_inc
v_inc
ggsave(here::here("figures","inc_villages.png"), height = 6, width = 8)

# ---------------------------------------------------------------------------- #
# Check consistency of village populations from raster vs case data

summary(v_shp_vl$pop_raster_agg[v_shp_vl$pop_care_estimate > 0])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0    2088    4150    6969    8411 1766275 
summary(v_shp_vl$pop_care_estimate[v_shp_vl$pop_care_estimate > 0])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 21    1764    3281    4713    6110   87258 

v_shp_vl %>% 
  filter(pop_care_estimate > 0) %>% 
  ggplot(aes(pop_care_estimate, pop_raster_agg)) +
  geom_abline() +
  geom_point(alpha = 0.3) + 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(x = "CARE estimate", y = "Aggregated WorldPop estimate")
ggsave(here::here("figures/cleaning","care_vs_raster_pops.png"), height = 5, width = 6)

# ---------------------------------------------------------------------------- #
# Check how many villages with cases but estimated to have zero population

v_shp_vl %>% 
  filter(X2018 > 0 & pop_raster_agg == 0) %>% 
  nrow() # 0

# ---------------------------------------------------------------------------- #
# Simplify polygons

v_shp_simp <- ms_simplify(v_shp, 
                          keep = 0.0001,
                          keep_shapes = FALSE) %>% 
  sfheaders::sf_remove_holes()

plot_map(v_shp_simp, strokecolor = '#097FB3',
         fillcolor = '#AED3E4')

# Summarise village polygon areas
summary(st_area(v_shp)/1000000)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0003   0.5644   1.2006   2.1029   2.4283 319.5798 

summary(st_area(v_shp_simp)/1000000)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.493   1.158   2.199   2.530 331.970 

# ---------------------------------------------------------------------------- #
# Define centroid geometry

v_cent_vl <- v_shp_vl %>%
  st_transform(7759) %>% 
  st_centroid()  

coo <- v_cent_vl %>% 
  st_transform(4326) %>% 
  st_coordinates() %>% 
  as.data.frame() 

v_cent_vl <- bind_cols(v_cent_vl, coo) 

# ---------------------------------------------------------------------------- #
# Aggregate to block

b_shp_vl <- v_shp_vl %>%
  left_join(blk_lookup) %>% 
  group_by(DIST_NAME, BLK_NAME) %>%
  summarise(across(X2013:X2018, sum, na.rm = TRUE),
            pop_raster_agg = sum(pop_raster_agg, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(X2018_inc = X2018/pop_raster_agg) %>%
  dplyr::select(DIST_NAME, BLK_NAME, pop_raster_agg, X2013:X2018, X2018_inc, geometry) %>%
  nngeo::st_remove_holes()

# Join block shapes with point data
b_shp_vl <- b_shp %>%
  st_join(vl_sf, join = st_intersects)

# Sum incidence across villages which fall within each block
b_shp_vl <- b_shp_vl %>%
  group_by(OBJECTID, kamis_master_dist, kamis_master_block, pop_raster_agg) %>%
  summarise(across(X2013:X2018, sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(across(X2013:X2018,
                .fns = function(x) x/pop_raster_agg,
                .names = "{.col}_inc_blk")) %>%
  dplyr::select(OBJECTID, kamis_master_dist, kamis_master_block, pop_raster_agg, X2013:X2018, X2013_inc_blk:X2018_inc_blk, geometry)

ggplot(b_shp_vl) +
  geom_sf(aes(geometry = geometry, fill = X2018_inc*1e4), col = NA) +
  scale_fill_viridis_c(trans = "identity", na.value = "white", direction = 1) + 
  labs(fill = "Incidence\nper 10,000") -> blk_inc_2018
blk_inc_2018
ggsave(here::here("figures","blk_inc_2018.png"), blk_inc_2018, height = 6, width = 8)

# vl_bh %>%
#   group_by(district, block) %>%
#   summarise(across(X2013:X2018, sum, na.rm = TRUE)) %>%
#   ungroup() -> vl_block_yr
# 
# b_shp %>% 
#   full_join(vl_block_yr, by = c("kamis_master_dist" = "district",
#                                 "kamis_master_block" = "block")) %>%
#   mutate(X2018_inc_blk = replace_na(X2018/pop_raster_agg, 0)) -> b_shp_vl

# mchdnms <- read_csv(file.path(datadir, "MatchedBlockNames.csv")) %>% 
#   # dplyr::select(district, admin3, kamis_master_dist, kamis_master_block, District1, Block1, DI) %>% 
#   dplyr::mutate(across(everything(), toupper))
# 
# b_shp_vl %>% 
#   st_drop_geometry() %>%
#   dplyr::select(district, admin3, kamis_master_dist, kamis_master_block, X2018_inc_blk) %>% 
#   dplyr::left_join(mchdnms) %>% #View()
#   full_join(v_cent_vl, by = c("District2" = "DIST_NAME",
#                               "Block2" = "BLK_NAME")) -> v_cent_vl_wblk


# ---------------------------------------------------------------------------- #
# Add block incidence to village centroid data
# 
# # Village centroids which don't intersect with a block polygon
# outside6 <- sapply(st_intersects(v_cent_vl, b_shp_vl),function(x){length(x)==0})
# summary(outside6)
# #    Mode   FALSE    TRUE 
# # logical   44600     194
# 
# v_cent_vl[outside6,] %>%
#   ggplot() +
#   geom_sf(data = boundary) +
#   geom_sf()
# 
# orig <- v_cent_vl
# v_cent_vl[outside6,] <- st_snap(v_cent_vl[outside6,], b_shp_vl, tolerance = units::set_units(500, m))
# 
# outside7 <- sapply(st_intersects(v_cent_vl, b_shp_vl),function(x){length(x)==0})
# summary(outside7)
# #    Mode   FALSE    TRUE 
# # logical   44739      55
# 
# v_cent_vl[outside7,] <- st_snap(v_cent_vl[outside7,], b_shp_vl, tolerance = units::set_units(1000, m))
# 
# outside8 <- sapply(st_intersects(v_cent_vl, b_shp_vl),function(x){length(x)==0})
# summary(outside8)
# #    Mode   FALSE    TRUE 
# # logical   44787       7
# 
# v_cent_vl[outside8,] <- st_snap(v_cent_vl[outside8,], b_shp_vl, tolerance = units::set_units(2000, m))
# 
# outside9 <- sapply(st_intersects(v_cent_vl, b_shp_vl),function(x){length(x)==0})
# summary(outside9)
# #    Mode   FALSE    TRUE 
# # logical   44793       1
# 
# v_cent_vl[outside9,] <- st_snap(v_cent_vl[outside9,], b_shp_vl, tolerance = units::set_units(2500, m))
# 
# outside10 <- sapply(st_intersects(v_cent_vl, b_shp_vl),function(x){length(x)==0})
# summary(outside10)
# 
# v_cent_vl[outside6,] %>%
#   ggplot() +
#   geom_sf(data = boundary) +
#   geom_sf(data = orig[outside6, ], col = "red") +
#   geom_sf(col = "green")
# ggsave(here::here("figures","cleaning","snapped_centroids_bshp.png"), height = 6, width = 8)
# 
# sf::sf_use_s2(TRUE)
# 
# # v_cent_vl_wblk <- b_shp_vl %>%
# #   dplyr::select(kamis_master_dist, kamis_master_block, X2018_inc_blk) %>% 
# #   st_join(v_cent_vl, join = st_intersects)
# 
# v_cent_vl_wblk <- v_cent_vl %>%
#   st_join(dplyr::select(b_shp_vl, kamis_master_dist, kamis_master_block, X2013_inc_blk:X2018_inc_blk), 
#           join = st_intersects)
# 
# # Has broadly worked:
# plot(v_cent_vl_wblk["X2018_inc_blk"])
# 
# # But polygons may overlap so a point can fall within more than one polygon, and
# # this ends up duplicating villages
# dupids <- v_cent_vl_wblk$OBJECTID[duplicated(v_cent_vl_wblk$OBJECTID)]
# dupids
# # 399  2739  8768  9234 14781 19838 20529 20530
# 
# View(v_cent_vl_wblk[v_cent_vl_wblk$OBJECTID %in% dupids,])
# 
# # As there are only few, manually remove duplicates based on block specified in 
# # the village shapefile
# 
# rmv <- which((v_cent_vl_wblk$OBJECTID == 399 & v_cent_vl_wblk$kamis_master_block == "KURSAKANTA")|
#                (v_cent_vl_wblk$OBJECTID == 2739 & v_cent_vl_wblk$kamis_master_block == "NABINAGAR")|
#                (v_cent_vl_wblk$OBJECTID == 8768 & v_cent_vl_wblk$kamis_master_block == "BUXAR")|
#                (v_cent_vl_wblk$OBJECTID == 9234 & v_cent_vl_wblk$kamis_master_block == "CHAKKI")|
#                (v_cent_vl_wblk$OBJECTID == 14781 & v_cent_vl_wblk$kamis_master_block == "GOPALGANJ SADAR")|
#                (v_cent_vl_wblk$OBJECTID == 19838 & v_cent_vl_wblk$kamis_master_block == "BARSOI")|
#                (v_cent_vl_wblk$OBJECTID %in% c(20529,20530) & v_cent_vl_wblk$kamis_master_block == "BAISA"))
# 
# v_cent_vl_wblk <- v_cent_vl_wblk[-rmv,]

################################################################################
# Save datasets

saveRDS(vl_sf, file.path(outdir,"vl_village_2018.rds"))
saveRDS(v_shp_vl, here::here(outdir, "v_shp_vl.rds"))
saveRDS(v_cent_vl, here::here(outdir, "v_cent_vl.rds"))
saveRDS(v_cent_vl_wblk, here::here(outdir, "v_cent_vl_wblk.rds"))
# saveRDS(vl_block_yr, here::here("data","aggregate","vl_block_2018.rds"))
saveRDS(b_shp_vl, here::here("data","aggregate","b_shp_vl.rds"))

################################################################################
################################################################################
