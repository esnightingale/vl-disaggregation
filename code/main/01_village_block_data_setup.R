library(sf)
library(raster)
library(tidyverse)
library(patchwork)

# Turn off s2 processing to avoid invalid spherical geometry errors
sf::sf_use_s2(FALSE)

theme_set(theme_minimal())

# Fix default ggsave background
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

# Local data folder
datadir <- "C:/Users/phpuenig/Documents/VL/Data"

# Output folder
outdir <- file.path(datadir, "KAMIS/Clean/village")

################################################################################
## STEP 1: Add village cases to polygons
################################################################################

# Village polygons
v_shp <- sf::read_sf(dsn = here::here(datadir,"Shapefiles","official_bihar_india_village_boundary_shapefile"),
                     layer = "Bihar_Village_Boundary") %>%
  dplyr::mutate(DIST_NAME = as.character(DIST_NAME),
                DIST_NAME = stringr::str_replace(DIST_NAME, '\\*', 'AN')) %>% 
  dplyr::select(OBJECTID, DIST_NAME, BLK_NAME, BLK_C_2011, GPNAME_1, V_CODE2011, V_NAME, geometry) %>% 
  mutate(across(DIST_NAME:V_NAME, toupper)) %>% 
  st_transform(7759) 

# Fix one pesky GP with special characters
v_shp$GPNAME_1[v_shp$DIST_NAME == "MUZAFFARPUR" & 
                 v_shp$BLK_NAME == "SAKRA" & 
                 v_shp$V_NAME == "DONWAN"] <- "RAGHUNATHPUR DONMA"

# State boundary
boundary <- v_shp %>% 
  sf::st_union() %>% 
  sfheaders::sf_remove_holes()
# plot(boundary)

# Village cases 2018
vl_bh <- read.csv(here::here(datadir,"KAMIS","Raw","village","village level data-Bi+Jh.csv"), header=TRUE) %>% 
  filter(state == "BIHAR") %>% 
  dplyr::select(vil_code, district, block, hsc, village, population, latitude, longitude, X2013:X2018, Total_VL) %>% 
  mutate(dist_blk = paste(district, block, sep = ":"))

# ---------------------------------------------------------------------------- #
# Check village polygons

summary(as.numeric(v_shp$BLK_C_2011))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0    1183    1309    1305    1450    1546 

# One village marked as unidentified
View(v_shp[v_shp$BLK_C_2011 == 0,])

ggplot(v_shp) +
  geom_sf(aes(fill = (BLK_C_2011 == 0)), col = NA)

# Read official block shapes to identify block
blocks <- readRDS(here::here("data/geography","bihar_block.rds"))
st_centroid(v_shp[v_shp$BLK_C_2011 == 0,]) %>% 
  st_transform(4326) %>% 
  st_join(blocks) -> unkn_block

unkn_block %>% 
  dplyr::select(district, admin3, kamis_master_dist, kamis_master_block)

# district admin3   kamis_master_dist kamis_master_block            geometry
# <chr>    <chr>    <chr>             <chr>                      <POINT [°]>
# SARAN    CHAPRA_S SARAN             CHAPRA             (84.69903 25.72466)

v_shp %>% 
  st_drop_geometry() %>% 
  filter(DIST_NAME == "SARAN" & BLK_NAME == "CHHAPRA") %>%
  distinct() %>% View()

v_shp %>% 
  st_drop_geometry() %>% 
  filter(BLK_C_2011 == "01258") %>%
  distinct() %>% View()

# Shape falls within Chapra block, defined as CHHAPRA in v_shp with code 01258

v_shp$DIST_NAME[v_shp$BLK_C_2011 == 0] <- "SARAN"
v_shp$BLK_NAME[v_shp$BLK_C_2011 == 0] <- "CHHAPRA"
v_shp$BLK_C_2011[v_shp$BLK_C_2011 == 0] <- "01258"

# ---------------------------------------------------------------------------- #
# Affected villages without GPS

# How many?
summary(is.na(vl_bh$latitude[vl_bh$X2018 > 0]))
#    Mode   FALSE    TRUE 
# logical    2126      60 

# Split villages with and without GPS
vl_gps <-  vl_bh %>% 
  filter(!(is.na(longitude)|is.na(latitude) & X2018 > 0)) 

# Pull missing GPS just for 2018 affected villages 
vl_nogps <- vl_bh %>% 
  filter((is.na(longitude)|is.na(latitude)) & X2018 > 0)

# Manually match by district/block/GP/village name
vl_bh %>% 
  filter(X2018 > 0 & is.na(longitude)) %>% 
  group_by(dist_blk) %>% 
  summarise(n = sum(X2018),
            n_vil = n_distinct(vil_code)) -> missgps_byblk

# File with matched names
match <- read_csv(here::here("data","village_nogps_namematch.csv"))

# Merge to shapefile on matched names, and generate coordinates from shape centroid
vl_nogps %>% 
  left_join(match) %>% 
  left_join(v_shp) %>% 
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
# Large GPS errors

buffer <- boundary %>% 
  sf::st_buffer(1e4)

ggplot() +
  geom_sf(data = boundary) +
  geom_sf(data = buffer, fill = NA) +
  geom_sf(data = vl_sf) +
  theme_minimal()
# ggsave(here::here("figures","cleaning","points_10km_outside_state.png"), height = 6, width = 8)

farout <- sapply(st_intersects(vl_sf, buffer),function(x){length(x)==0})
# View(vl_sf[farout,])

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
# Marginal GPS errors 

# Village locations which don't intersect with a polygon
outside <- sapply(st_intersects(vl_sf, v_shp),function(x){length(x)==0})
summary(outside)
#    Mode   FALSE    TRUE 
# logical    9956       8 

# How many affected?
sum(vl_sf$X2018[outside] > 0) # 2

vl_sf[outside,] %>%
  ggplot() +
  geom_sf(data = boundary) +
  geom_sf()
ggsave(here::here("figures","cleaning","points_outside_polygons.png"), height = 6, width = 8)

orig <- vl_sf
vl_sf[outside,] <- st_snap(vl_sf[outside,], v_shp, tolerance = units::set_units(500, m))

outside2 <- sapply(st_intersects(vl_sf, v_shp),function(x){length(x)==0})
summary(outside2)
sum(vl_sf$X2018[outside2]) # 0

vl_sf[outside2,] %>%
  ggplot() +
  geom_sf(data = boundary) +
  geom_sf(data = orig[outside2, ], col = "red", cex = 0.5) +
  geom_sf(col = "green", cex = 0.5)
ggsave(here::here("figures","cleaning","snapped_points.png"), height = 6, width = 8)

# ---------------------------------------------------------------------------- #
# Overlay and aggregate cases to polygons

v_shp_vl <- v_shp %>% 
  st_join(vl_sf, join = st_intersects) %>% 
  group_by(OBJECTID, DIST_NAME, BLK_NAME, BLK_C_2011, GPNAME_1, V_NAME) %>% 
  summarise(across(X2013:X2018, sum, na.rm = TRUE),
            pop_care_estimate = sum(population, na.rm = TRUE)) %>% 
  ungroup() 

attr(v_shp_vl, "agr") <- NULL

################################################################################
## STEP 2: Add village populations to polygons
################################################################################

# Bihar population raster (cropped from WorldPop India 100m)
popBH <- raster::raster(here::here("data","covariates","pop_100m.tif"))

v_shp_vl$pop_raster_agg <- exactextractr::exact_extract(x = popBH, y = v_shp_vl, fun = "sum")
v_shp_vl$X2018_inc <- v_shp_vl$X2018/v_shp_vl$pop_raster_agg

# Check consistency of populations from raster vs CARE estimates

summary(v_shp_vl$pop_raster_agg[v_shp_vl$pop_care_estimate > 0])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0    1984    3764    6187    7555 1741401
summary(v_shp_vl$pop_care_estimate[v_shp_vl$pop_care_estimate > 0])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   14    1723    3156    4481    5795   66859 

v_shp_vl[which.max(v_shp_vl$pop_raster_agg),]

v_shp_vl %>% 
  filter(pop_care_estimate > 0) %>% 
  ggplot(aes(pop_care_estimate, pop_raster_agg)) +
  geom_abline() +
  geom_point(alpha = 0.3) + 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(x = "CARE estimate", y = "Aggregated WorldPop estimate")
ggsave(here::here("figures/cleaning","care_vs_raster_pops.png"), height = 5, width = 6)

# How many villages with cases estimated to have zero population
v_shp_vl %>% 
  filter(X2018 > 0 & pop_raster_agg == 0) %>% 
  nrow() # 0

ggplot(v_shp_vl, aes(geometry = geometry, fill = pop_raster_agg)) +
  geom_sf(data = boundary, fill = "grey") +
  geom_sf(col = NA) +
  scale_fill_viridis_c(trans = "sqrt") +
  labs(fill = "Population") -> v_pops

ggplot(v_shp_vl, aes(geometry = geometry, fill = X2018*1000/pop_raster_agg)) +
  geom_sf(data = boundary, fill = "grey") +
  geom_sf(col = NA) +
  scale_fill_viridis_c(trans = "sqrt") +
  labs(fill = "Incidence/1,000") -> v_inc

v_pops + v_inc
ggsave(here::here("figures","vl_villages.png"), height = 6, width = 14)

# ---------------------------------------------------------------------------- #
# Compare with simplifying first then extracting populations

v_simp_vl <- v_shp_vl %>% 
  rmapshaper::ms_simplify(keep = 0.001, keep_shapes = FALSE)

v_simp_vl$pop_raster_agg <- exactextractr::exact_extract(x = popBH, y = v_simp_vl, fun = "sum")
v_simp_vl$X2018_inc <- v_simp_vl$X2018/v_simp_vl$pop_raster_agg

# Check consistency of populations from raster vs CARE estimates

summary(v_simp_vl$pop_raster_agg[v_simp_vl$pop_care_estimate > 0])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0    1840    3758    6488    7989 1766275 
summary(v_simp_vl$pop_care_estimate[v_simp_vl$pop_care_estimate > 0])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   14    1723    3156    4481    5795   66859 

v_simp_vl %>% 
  filter(X2018 > 0 & pop_raster_agg == 0) %>% 
  nrow() # 0

################################################################################
## STEP 3: Aggregate to blocks
################################################################################

n_distinct(v_shp_vl$BLK_C_2011) # 533

b_shp_vl <- v_shp_vl %>% 
  group_by(BLK_C_2011) %>% 
  summarise(across(X2013:X2018, sum, na.rm = TRUE),
            pop_raster_agg = sum(pop_raster_agg, na.rm = TRUE)) %>% 
  mutate(X2018_inc_blk = X2018/pop_raster_agg) %>% 
  ungroup()

# blk_coo <- b_shp_vl %>% 
#   st_centroid() %>% 
#   st_coordinates()
# 
# pdf(here::here("figures/cleaning","block_codes_2011.pdf"), height = 15, width = 20)
# b_shp_vl %>% 
#   ggplot(aes(geometry = geometry)) +
#   geom_sf(col = NA) +
#   geom_text(aes(label = BLK_C_2011, x = blk_coo$X, y = blk_coo$Y)) +
#   labs(x = "", y = "")
# dev.off()

summary(b_shp_vl$X2018_inc_blk*1e4)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.00000 0.08593 0.30295 0.31700 5.76799

ggplot(b_shp_vl, aes(geometry = geometry, fill = pop_raster_agg)) +
  geom_sf(data = boundary, fill = "grey") +
  geom_sf(col = NA) +
  scale_fill_viridis_c(trans = "sqrt") +
  labs(fill = "Population") -> b_pops

ggplot(b_shp_vl, aes(geometry = geometry, fill = X2018*1e4/pop_raster_agg)) +
  geom_sf(data = boundary, fill = "grey") +
  geom_sf(col = NA) +
  scale_fill_viridis_c(trans = "sqrt") +
  labs(fill = "Incidence/10,000") -> b_inc

b_pops + b_inc
ggsave(here::here("figures","vl_blocks.png"), height = 6, width = 14)

################################################################################
## STEP 4: Add block incidence to village data
################################################################################

blk_inc <- b_shp_vl %>% 
  st_drop_geometry() %>% 
  dplyr::select(BLK_C_2011, X2018_inc_blk)

v_shp_vl <- v_shp_vl %>% 
  left_join(blk_inc)

ggplot(v_shp_vl, aes(geometry = geometry, fill = X2018_inc_blk*pop_raster_agg)) +
  geom_sf(data = boundary, fill = "grey") +
  geom_sf(col = NA) +
  scale_fill_viridis_c(trans = "sqrt") +
  labs(fill = "Expected\nvillage incidence") 
ggsave(here::here("figures","cleaning","vl_villages_expected.png"), height = 6, width = 8)

################################################################################
## STEP 5: Add covariates to village data
################################################################################

# Covariate raster stack
covariate_stack <- readRDS(here::here("data","covariates","analysis","cov_stack_1km_std.rds"))

# Drop population raster
names(covariate_stack)
covariate_stack <- covariate_stack[[-1]]

covs <- exactextractr::exact_extract(covariate_stack, v_shp_vl, fun = "mean")
names(covs) <- gsub("mean.","", names(covs))

v_shp_vl <- bind_cols(v_shp_vl, covs) 

# Define centroid geometry
v_cent_vl <- v_shp_vl %>%
  st_transform(7759) %>% 
  st_centroid()  

coo <- v_cent_vl %>% 
  st_transform(4326) %>% 
  st_coordinates() %>% 
  as.data.frame() 

v_cent_vl <- bind_cols(v_cent_vl, coo) 
attr(v_cent_vl, "agr") <- NULL

################################################################################
################################################################################
# SAVE ANALYSIS DATA

saveRDS(v_shp_vl, here::here("data","analysis","v_shp_vl.rds"))
saveRDS(v_cent_vl, here::here("data","analysis","v_cent_vl.rds"))
saveRDS(b_shp_vl, here::here("data","analysis","b_shp_vl.rds"))
saveRDS(boundary, here::here("data","analysis","boundary.rds"))

################################################################################
################################################################################


