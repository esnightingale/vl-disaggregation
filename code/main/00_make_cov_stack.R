################################################################################
# Read processed covariate rasters and join into a stack for analysis
################################################################################

library(tidyverse)
library(sf)
library(raster)
library(gdalUtils)

################################################################################

files <- c("pop_100m","elev_100m","dist_majroad_100m","dist_water_100m", "built_ext_100m",
           "cityaccess",
           "lst_mean_modis","lst_sd_modis","ndvi_mean_modis","ndvi_sd_modis") #"land_cover_9cat"

r.list <- lapply(files, 
                 function(f) raster::raster(here::here("data","covariates","analysis",paste0(f, ".tif"))))
names(r.list) <- files

# Resample worldpop vars to the same resolution as modis (1km) 
for(r in 1:6){
  r.list[[r]] <- raster::resample(r.list[[r]], r.list$lst_mean_modis) 
}

# Remove 100m and modis tags from names
names(r.list) <- gsub("_100m","",names(r.list))
names(r.list) <- gsub("_modis","",names(r.list))

lapply(r.list, summary)

# Log-transform distance to water
# r.list$dist_water <- log(r.list$dist_water)

stk <- raster::stack(r.list)

# Standardise covariate layers, but excluding population 
r.list.std <- r.list
r.list.std[-1] <- lapply(r.list[-1], raster::scale)
stk.std <- raster::stack(r.list.std)

################################################################################

saveRDS(r.list, here::here("data","covariates","analysis","raster_list_1km_std.rds"))
saveRDS(r.list.std, here::here("data","covariates","analysis","raster_list_1km_std.rds"))
saveRDS(stk, here::here("data","covariates","analysis","cov_stack_1km.rds"))
saveRDS(stk.std, here::here("data","covariates","analysis","cov_stack_1km_std.rds"))

################################################################################
