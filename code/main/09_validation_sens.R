################################################################################
################################################################################

library(disaggregation)
library(raster)
library(tidyverse)
library(sf)
library(patchwork)

# Fix default ggsave background
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
theme_set(theme_minimal(base_size = 16))

list.files(here::here("code","utils"), full.names = TRUE) %>% purrr::walk(source)

# Local data folder
datadir <- "C:/Users/phpuenig/Documents/VL/Data"
figdir <- "figures/disaggregation/predictions"

v_shp_vl <- readRDS(here::here("data","analysis","v_shp_vl.rds")) %>% 
  st_transform(4326)
b_shp_vl <- readRDS(here::here("data","analysis","b_shp_vl.rds")) %>% 
  st_transform(4326)
boundary <- readRDS(here::here("data","analysis","boundary.rds")) %>% 
  st_transform(4326)

v_shp_vl %>% 
  st_drop_geometry() -> v_df

# Disaggregation predictions
predictions <- readRDS(here::here("output","disag_predictions.rds"))

# Random forest fit
fit_rf <- readRDS(here::here("output","fit_rf.rds"))

# Population raster for prediction
population_raster <- readRDS(here::here("data","analysis","population_raster.rds"))

################################################################################

models <- c("Baseline","Disaggregation - IID only", "Disaggregation - Full","Village level")

# Multiply predicted incidence by population per pixel, then aggregate cases per shape
pred_cases <- stack(lapply(predictions, get_cases))

pred_cases_village <- exactextractr::exact_extract(pred_cases, 
                                                    v_shp_vl, 
                                                    fun = "sum")
# Add predictions for each village
v_shp_vl_pred <- v_shp_vl %>% 
  mutate(obs_inc = X2018*1000/na_if(pop_care_estimate,0),
         pred_inc_bl = X2018_inc_blk*1000,
         pred_inc_iid = pred_cases_village$sum.IID.only*1000/na_if(pop_care_estimate,0),
         pred_inc_full = pred_cases_village$sum.Full*1000/na_if(pop_care_estimate,0),
         pred_inc_rf = fit_rf$predicted*1000/na_if(pop_care_estimate,0))

saveRDS(v_shp_vl_pred, here::here("output","v_shp_vl_wpreds_sens.rds"))

# ---------------------------------------------------------------------------- #

# Overall errors
v_shp_vl_pred_df <- st_drop_geometry(v_shp_vl_pred)

errors_all <- bind_rows(
  apply(dplyr::select(v_shp_vl_pred_df, pred_inc_bl:pred_inc_rf), 2, 
        function(pred) get_errors2(v_shp_vl_pred_df, pred, type = "sens"))
) %>% 
  mutate(Model = models)
write.csv(errors_all, here::here("output","errors_all_sens.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------- #

# Plot all predictions

make_plot <- function(obs, pred, title = NULL, rho = NULL, xlims = NULL){
  
  p1 <- plot_scatter(obs, pred, rho = rho, title = title, xlims = xlims)
  p2 <- plot_box(obs, pred, xlims = xlims)
  
  p <- p1 + p2
  return(p)
}

round(range(
  v_shp_vl_pred %>%
    pivot_longer(pred_inc_bl:pred_inc_rf) %>% 
    pull(value), 
  na.rm = T),4)
# 0.0000 32.5895

p_bl <- make_plot(v_shp_vl_pred$obs_inc, v_shp_vl_pred$pred_inc_bl, xlims = c(0.00001,750),
                  title = "Baseline", rho = errors_all$rho[errors_all$Model == "Baseline"])
p_dis1 <- make_plot(v_shp_vl_pred$obs_inc, v_shp_vl_pred$pred_inc_iid, xlims = c(0.00001,750),
                    title = "Disaggregation - without field", rho = errors_all$rho[errors_all$Model == "Disaggregation - without field"])
p_dis2 <- make_plot(v_shp_vl_pred$obs_inc, v_shp_vl_pred$pred_inc_full, xlims = c(0.00001,750),
                    title = "Disaggregation - Full", rho = errors_all$rho[errors_all$Model == "Disaggregation - Full"])
p_rf <- make_plot(v_shp_vl_pred$obs_inc, v_shp_vl_pred$pred_inc_rf, xlims = c(0.00001,750),
                  title = "Village-level random forest", rho = errors_all$rho[errors_all$Model == "Village-level random forest"])
p_list <- list(p_bl, p_dis1, p_dis2, p_rf)

p_lab1 <- 
  ggplot() + 
  annotate(geom = "text", x = 1, y = 1, 
           size = 6, angle = 90,
           label = "Observed incidence, per 1,000") +
  coord_cartesian(clip = "off")+
  theme_void()
p_lab2 <- 
  ggplot() + 
  annotate(geom = "text", x = 1, y = 1, 
           size = 6,
           label = "Predicted incidence, per 1,000") +
  coord_cartesian(clip = "off")+
  theme_void()

(p_lab1 | wrap_plots(p_list, nrow = 4)) +
  plot_layout(widths = c(0.02, 1)) -> p_

p_ / p_lab2 +
  plot_layout(heights = c(1, 0.02)) -> p_final

p_final
ggsave(here::here(figdir, paste0("vil_obsvpred_all_sens.png")), p_final, height = 12, width = 10, dpi = 400)
ggsave(here::here("figures/manuscript/supplementary/FigS6.png"), p_final, height = 12, width = 10, dpi = 400)

################################################################################
