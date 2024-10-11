################################################################################
################################################################################
# Description: Fit block disaggregation model to 2018 data
################################################################################

library(disaggregation)
library(raster)
library(ggplot2)
library(dplyr)
library(sf)

theme_set(theme_minimal())
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

# Block polygons with 2018 case counts + populations
b_shp_vl <- readRDS(here::here("data","aggregate","b_shp_vl.rds")) %>% 
  st_transform(4326)

boundary <- as_Spatial(sf::st_union(b_shp_vl))
b_shp_vl <- as_Spatial(b_shp_vl)

crsBH <- st_crs(4326)

# ---------------------------------------------------------------------------- #
# Read covariate raster stack

# Incl: elevation, land surface temp, NDVI, distance to major road and distance 
# to water body.

covariate_stack <- readRDS(here::here("data","covariates","analysis","cov_stack_1km_std.rds")) %>% 
  raster::crop(b_shp_vl)

names(covariate_stack)
# Non-time-varying:
# "pop"          "elev"         "dist_majroad" "dist_water"   "cityaccess"  
# 2018-specific:
# "lst_mean"     "lst_sd"   "ndvi_mean"    "ndvi_sd"     

# Keep city travel time instead of distance to major road
covariate_stack <- raster::dropLayer(covariate_stack, "dist_majroad")
covariate_stack <- raster::dropLayer(covariate_stack, "built_ext")

res(covariate_stack)
# 0.008333333 0.008333333

e <- extent(covariate_stack)
e
# class      : Extent 
# xmin       : 83.325 
# xmax       : 88.3 
# ymin       : 24.28333 
# ymax       : 27.51667 

# Separate population raster
population_raster <- covariate_stack[[1]]
saveRDS(population_raster, here::here("data","analysis","population_raster.rds"))
covariate_stack <- covariate_stack[[2:nlayers(covariate_stack)]]

# ---------------------------------------------------------------------------- #
# Setup data

pars <- list(convex = -0.1, concave = -0.5, resolution = 200,
             max.edge = c(0.1, 0.5), cut = 0.02, offset = c(1, 1), crs = crsBH)

data_for_model <- prepare_data(polygon_shapefile = b_shp_vl, 
                               covariate_rasters = covariate_stack, 
                               aggregation_raster = population_raster,
                               mesh.args = pars,
                               id_var = 'OBJECTID',
                               response_var = 'X2018',
                               ncores = 8,
                               na.action = TRUE)
saveRDS(data_for_model, here::here("data","analysis","disag_data.rds"))

plot(data_for_model)
ggsave(here::here("figures","disaggregation","disag_setup.png"), height = 12, width = 15)

png(here::here("figures","disaggregation","covariates.png"), height = 900, width = 1000)
plot(data_for_model$covariate_rasters)
dev.off()

png(here::here("figures","disaggregation","mesh.png"), height = 800, width = 1000)
plot(data_for_model$mesh)
dev.off()

# ---------------------------------------------------------------------------- #
# Fit model

# Set priors
# Note: for intercept can judge appropriate mean from considering that VL incidence is generally low
# and this is on a log scale, so should be substantially < 0 but quite wide. 
priors = list(priormean_intercept = -4,
              priorsd_intercept = 8,
              priormean_slope = 0.0,
              priorsd_slope = 0.4,
              prior_rho_min = 0.1,
              prior_rho_prob = 0.01,
              prior_sigma_max = 2,
              prior_sigma_prob = 0.01,
              prior_iideffect_sd_max = 0.1,
              prior_iideffect_sd_prob = 0.01)

fit <- disag_model(data = data_for_model,
                   iterations = 1000,
                   family = 'poisson',
                   link = 'log',
                   iid = TRUE,
                   field = TRUE,
                   priors = priors,
                   silent = FALSE)
saveRDS(fit, here::here("output", "disag_fit.rds"))
summary(fit)

fit_iid <- disag_model(data = data_for_model,
                       iterations = 1000,
                       family = 'poisson',
                       link = 'log',
                       iid = TRUE,
                       field = FALSE,
                       priors = priors,
                       silent = FALSE)
saveRDS(fit_iid, here::here("output", "disag_fit_iid.rds"))
summary(fit_iid)

fit.list <- list(Full = fit, `IID only` = fit_iid)
saveRDS(fit.list, here::here("output", "disag_fits.rds"))

################################################################################
################################################################################