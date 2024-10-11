library(disaggregation)
library(tidyverse)
library(sf)

set.seed(1234)

# Fix default ggsave background
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
theme_set(theme_minimal())

# Local data folder
datadir <- "C:/Users/phpuenig/Documents/VL/Data"
figdir <- "figures/disaggregation/predictions"

v_shp_vl <- readRDS(here::here("data","analysis","v_shp_vl.rds")) %>% 
  st_transform(4326)

v_shp_vl %>% 
  st_drop_geometry() -> v_df

################################################################################

## Random Forest ##

coo <- v_shp_vl %>%
  st_transform(7759) %>% 
  st_centroid() %>% 
  st_transform(4326) %>% 
  st_coordinates() %>% 
  as.data.frame() 

v_df <- bind_cols(v_df, coo) 

# New dataset of only relevant vars, all numeric
d <- v_df %>% 
  dplyr::select(X2018, pop_raster_agg, 
                elev, dist_water, cityaccess, lst_mean, lst_sd, ndvi_mean, ndvi_sd, 
                X, Y) %>%
  mutate(across(everything(), as.numeric))

remove(v_shp_vl)

# Fit random forest models with different mtry values

# Default and maximum values
floor(ncol(d)/3) # 3
ncol(dplyr::select(d, -X2018)) # 10

m <- c(2, 3, 6, 10)
fits <- lapply(m, 
               function(m) randomForest::randomForest(y = d$X2018, 
                                                      x = d[,-1],
                                                      ntree = 200, 
                                                      data = d, 
                                                      mtry = m, 
                                                      importance = TRUE, 
                                                      proximity = FALSE))

saveRDS(fits, here::here("output","fits_rf.rds"))

lapply(fits, print)
# No. of variables tried at each split: 2
# 
# Mean of squared residuals: 0.3408659
# % Var explained: 6.6

# No. of variables tried at each split: 3
# 
# Mean of squared residuals: 0.3417122
# % Var explained: 6.37

# No. of variables tried at each split: 6
# 
# Mean of squared residuals: 0.3481108
# % Var explained: 4.62

# No. of variables tried at each split: 10
# 
# Mean of squared residuals: 0.3578995
# % Var explained: 1.94


lapply(fits, function(fit) summary(fit$mse))
# [[1]]
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3409  0.3421  0.3452  0.3590  0.3555  0.5870 
# 
# [[2]]
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3417  0.3437  0.3468  0.3585  0.3540  0.5969 
# 
# [[3]]
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3481  0.3497  0.3527  0.3649  0.3577  0.6815 
# 
# [[4]]
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3579  0.3597  0.3634  0.3810  0.3691  0.7249

fit <- fits[[2]]
saveRDS(fit, here::here("output","fit_rf.rds"))

################################################################################
################################################################################
