################################################################################
################################################################################

library(tidyverse)
library(sf)
library(raster)
library(patchwork)
library(disaggregation)

set.seed(1234)

# Fix default ggsave background
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
theme_set(theme_minimal(base_size = 16))
pal <- viridis::plasma(4, end = 0.85)

# Source helper functions
list.files(here::here("code","utils"), full.names = TRUE) %>% purrr::walk(source)

# Local data folder
datadir <- "C:/Users/phpuenig/Documents/VL/Data"
figdir <- "figures/disaggregation/predictions"

# VL incidence data
v_cent_vl <- readRDS(here::here("data","analysis","v_cent_vl.rds")) %>% 
  st_transform(4326)
v_shp_vl <- readRDS(here::here("data","analysis","v_shp_vl.rds")) %>% 
  st_transform(4326)
b_shp_vl <- readRDS(here::here("data","analysis","b_shp_vl.rds")) %>% 
  st_transform(4326)
boundary <- readRDS(here::here("data","analysis","boundary.rds")) %>% 
  st_transform(4326)

v_shp_vl %>% 
  st_drop_geometry() -> v_df

# Model predictions
predictions <- readRDS(here::here("output","disag_predictions.rds"))
fits_disag <- readRDS(here::here("output","disag_fits.rds"))

# Random forest fit
fit_rf <- readRDS(here::here("output","fit_rf.rds"))

# Population raster for prediction
population_raster <- readRDS(here::here("data","analysis","population_raster.rds"))

################################################################################

models <- c("Baseline","Disaggregation - without field", "Disaggregation - Full","Village-level random forest")

# Multiply predicted incidence by population per pixel, then aggregate cases per shape
pred_cases <- stack(lapply(predictions, get_cases_mean))

pred_cases_village <- exactextractr::exact_extract(pred_cases, 
                                                   v_shp_vl, 
                                                   fun = "sum")

# Add predictions for each village
v_shp_vl_pred <- v_shp_vl %>% 
  mutate(obs_inc = X2018*1000/na_if(pop_raster_agg,0),
         pred_inc_bl = X2018_inc_blk*1000,
         pred_inc_iid = pred_cases_village$sum.IID.only*1000/na_if(pop_raster_agg,0),
         pred_inc_full = pred_cases_village$sum.Full*1000/na_if(pop_raster_agg,0),
         pred_inc_rf = pred_rf*1000/na_if(pop_raster_agg,0))

# saveRDS(v_shp_vl_pred, here::here("output","v_shp_vl_wpreds.rds"))

# Summarise village predictions
summary(v_shp_vl_pred$obs_inc)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#   0.0000   0.0000   0.0000   0.0394   0.0000 332.3194       83 
summary(v_shp_vl_pred$pred_inc_bl)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000000 0.000000 0.005408 0.028910 0.028189 0.576798 
summary(v_shp_vl_pred$pred_inc_iid)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#   0.0000   0.0013   0.0057   0.0555   0.0252 739.2873       83 
summary(v_shp_vl_pred$pred_inc_full)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#   0.0000   0.0008   0.0066   0.0523   0.0269 618.4681       83 
summary(v_shp_vl_pred$pred_inc_rf)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
# 0.00000   0.00000   0.00725   0.07228   0.04413 161.96242        83 

# NAs in predictions from 0 estimated village population (block incidence not affected as this isn't dependent on village pop)

# Save predictions in table
v_shp_vl_pred %>% 
  st_drop_geometry() %>% 
  select(OBJECTID,elev:ndvi_sd,pop_raster_agg, pop_care_estimate,obs_inc:pred_inc_rf) -> pred_output 
write.csv(pred_output, here::here("output","final_predictions.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------- #
# Plot each set of predictions

ggplot(v_shp_vl_pred, aes(fill = X2018)) +
  geom_sf(col = NA) +
  scale_fill_viridis_c(trans = "sqrt", na.value = "grey", direction = 1, limits = c(0,16)) + 
  labs(fill = "Expected\ncase count") +
  theme(axis.text = element_blank()) -> map_obs

ggplot(v_shp_vl_pred, aes(fill = pred_inc_bl*pop_raster_agg/1000)) +
  geom_sf(col = NA) +
  scale_fill_viridis_c(trans = "sqrt", na.value = "grey", direction = 1, limits = c(0,16)) + 
  labs(fill = "Expected\ncase count") +
  theme(axis.text = element_blank()) -> map_bl

ggplot(v_shp_vl_pred, aes(fill = pred_inc_iid*pop_raster_agg/1000)) +
  geom_sf(col = NA) +
  scale_fill_viridis_c(trans = "sqrt", na.value = "grey", direction = 1, limits = c(0,16)) +  
  labs(fill = "Expected\ncase count") +
  theme(axis.text = element_blank()) -> map_disag_iid

ggplot(v_shp_vl_pred, aes(fill = pred_inc_full*pop_raster_agg/1000)) +
  geom_sf(col = NA) +
  scale_fill_viridis_c(trans = "sqrt", na.value = "grey", direction = 1, limits = c(0,16)) + 
  labs(fill = "Expected\ncase count") +
  theme(axis.text = element_blank()) -> map_disag

ggplot(v_shp_vl_pred, aes(fill = (pred_inc_rf*pop_raster_agg/1000)+0.0001)) +
  geom_sf(col = NA) +
  scale_fill_viridis_c(trans = "sqrt", na.value = "grey", direction = 1, limits = c(0,16)) + 
  labs(fill = "Expected\ncase count") +
  theme(axis.text = element_blank()) -> map_rf

plot.list <- list(obs = map_obs, baseline = map_bl, disag = map_disag, rf = map_rf)
purrr::map2(plot.list,
            names(plot.list),
            function(x, nm) ggsave(here::here(figdir,paste0("map_pred_",nm,".png")),
                                   x,
                                   height = 4, width = 5))

v_shp_vl_pred %>% 
  mutate(across(c(pred_inc_bl:pred_inc_rf), function(x) x*pop_raster_agg/1000)) %>% 
  pivot_longer(c(X2018,pred_inc_rf)) %>% 
  mutate(name = factor(name, levels = c("pred_inc_rf", "X2018"),
                       labels = c("Predicted","Observed"))) %>% 
  ggplot(aes(fill = value)) +
  geom_sf(col = NA) +
  scale_fill_viridis_c(trans = "sqrt", na.value = "grey", direction = 1) + 
  labs(fill = "Case count") +
  facet_wrap(~name) +
  theme(axis.text = element_blank()) -> map_comb

ggsave(here::here(figdir,"maps_pred_combined_rf.png"), map_comb, height = 4, width = 12)

(map_bl + map_disag_iid) / (map_disag + map_rf) + 
  plot_annotation(tag_levels = "a") + 
  plot_layout(guides = "collect") -> maps_combined

ggsave(here::here(figdir,"maps_pred_combined.png"), maps_combined, height = 10, width = 12)

# ---------------------------------------------------------------------------- #
# Calculate overall error metrics

v_shp_vl_pred_df <- st_drop_geometry(v_shp_vl_pred)

errors_all <- bind_rows(
  apply(dplyr::select(v_shp_vl_pred_df, pred_inc_bl:pred_inc_rf), 2, 
        function(pred) get_errors2(v_shp_vl_pred_df, pred, type = "main"))) %>% 
  mutate(Model = models) %>% 
  dplyr::select(Model, rmse, mae, sens, spec, ppv, everything())

write.csv(errors_all, here::here("output","errors_all.csv"), row.names = FALSE)

errors_plot <- errors_all %>% 
  mutate(Model = factor(c("Baseline","Disaggregation\nwithout field", "Disaggregation\nFull","Village-level\nRandom forest"),
                        levels = c("Baseline","Disaggregation\nwithout field", "Disaggregation\nFull","Village-level\nRandom forest")),
         type = "Overall")

errors_plot %>% 
  ggplot(aes(Model, rmse, col = Model)) +
  geom_point() +
  labs(x = NULL, y = "RMSE") +
  scale_colour_manual(values = pal) +
  guides(col = "none") -> plot_rmse

errors_plot %>% 
  ggplot(aes(Model, mae, col = Model)) +
  geom_point() +
  labs(x = NULL, y = "MAE") +
  scale_colour_manual(values = pal) +
  guides(col = "none") -> plot_mae

errors_plot %>% 
  ggplot(aes(Model, rho, ymin = rho_lo, ymax = rho_hi, col = Model)) +
  geom_errorbar(width = 0.2) +
  geom_point() +
  labs(x = NULL, y = "Spearman's rho") +
  scale_colour_manual(values = pal) +
  guides(col = "none") -> plot_rho

errors_plot %>% 
  ggplot(aes(Model, ppv, col = Model)) +
  geom_point() +
  labs(x = NULL, y = "PPV") +
  scale_colour_manual(values = pal) +
  guides(col = "none") -> plot_ppv

errors_plot %>% 
  ggplot(aes(Model, perc_agree_affect, col = Model)) +
  geom_point(position = position_dodge(width = 0.2)) +
  labs(x = NULL,
       # x = "Block endemicity",
       y = "% Agreement (cases > 0)") +
  scale_colour_manual(values = pal) +
  guides(col = "none") -> agreeaffect_byendm

(plot_rmse + plot_mae) / (plot_rho + agreeaffect_byendm)
ggsave(here::here(figdir,"plot_errors_overall.png"), height = 8, width = 10)

# ---------------------------------------------------------------------------- #
# Summarise accuracy excluding zero blocks

v_shp_vl_pred_df %>% 
  mutate(block_zero = (X2018_inc_blk == 0)) %>% 
  group_by(block_zero) %>% 
  group_split() -> by_nz

sapply(by_nz, nrow)
# 25919 18875

get_all_errors <- function(dat){
  bind_rows(
    apply(dplyr::select(dat, pred_inc_bl:pred_inc_rf), 2, 
          function(preds) get_errors2(dat, preds = preds, type = "main"))) %>% 
    mutate(Model = models,
           group = unique(dat$block_zero))
}

errors_all_bynz <- bind_rows(lapply(by_nz, get_all_errors)) %>% 
  mutate(Model = factor(Model, levels = models),
         group = factor(group, levels = c(FALSE,TRUE), labels = c("Block non-zero", "Block zero")))

write.csv(errors_all_bynz, here::here("output","errors_all_bynonzero.csv"), row.names = FALSE)

errors_all_nonzero <- filter(errors_all_bynz, group == "Block non-zero") %>% 
  mutate(Model = factor(c("Baseline","Disaggregation\nwithout field", "Disaggregation\nFull","Village-level\nRandom forest"),
                        levels = c("Baseline","Disaggregation\nwithout field", "Disaggregation\nFull","Village-level\nRandom forest")),
         type = "Excl. zero blocks")

errors_comb <- errors_plot %>% 
  bind_rows(errors_all_nonzero)

errors_comb %>% 
  ggplot(aes(Model, rmse, col = Model, shape = type)) +
  geom_point(position = position_dodge(width = 0.2)) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values = c(1,16)) +
  labs(x = NULL,
       y = NULL, 
       title = "Root Mean Squared Error (RMSE)",
       shape = NULL) +
  guides(col = "none") -> rmse_byendm

errors_comb %>% 
  ggplot(aes(Model, mae, col = Model, shape = type)) +
  geom_point(position = position_dodge(width = 0.2)) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values = c(1,16)) +
  labs(x = NULL,
       y = NULL,
       title = "Mean Absolute Error (MAE)",
       shape = NULL) +
  guides(col = "none") -> mae_byendm

errors_comb %>% 
  ggplot(aes(Model, rho, ymin = rho_lo, ymax = rho_hi, col = Model, shape = type, lty = type)) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2)) +
  labs(x = NULL,
       y = NULL,
       title = expression("Spearman's " * rho),
       shape = NULL) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values = c(1,16)) +
  scale_linetype_manual(values = c("dashed","solid")) +
  guides(col = "none", lty = "none") -> rho_byendm

errors_comb %>% 
  ggplot(aes(Model, ppv, col = Model, shape = type)) +
  geom_point(position = position_dodge(width = 0.2)) +
  labs(x = NULL,
       y = NULL,
       title = "Positive predictive value (PPV) - case count > 0",
       shape = NULL) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values = c(1,16)) +
  guides(col = "none") -> ppv_byendm

errors_comb %>% 
  ggplot(aes(Model, perc_agree_affect, col = Model, shape = type)) +
  geom_point(position = position_dodge(width = 0.2)) +
  labs(x = NULL,
       y = NULL,
       title = "% Agreement: Case count > 0",
       shape = NULL) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values = c(1,16)) +
  guides(col = "none") -> agreeaffect_byendm

(rmse_byendm + mae_byendm) / (rho_byendm + agreeaffect_byendm) + 
  plot_annotation(tag_levels = "a") +
  plot_layout(guides = "collect") & theme(legend.position='bottom')
ggsave(here::here(figdir,"plot_errors4_bynonzero.png"), height = 8, width = 10)

rmse_byendm + mae_byendm + rho_byendm + 
  plot_annotation(tag_levels = "a") + plot_layout(guides = "collect") & theme(legend.position='bottom')
ggsave(here::here(figdir,"plot_errors_bynonzero.png"), height = 5, width = 15)
ggsave(here::here("figures","manuscript","Fig5.png"), height = 6, width = 18, dpi = 300)

# ---------------------------------------------------------------------------- #
# Summarise accuracy by block endemicity

v_shp_vl_pred_df %>% 
  mutate(block_endm = factor(case_when(X2018_inc_blk*1e4 < 0.5 ~ "Low",
                                       (X2018_inc_blk*1e4 >= 0.5 & X2018_inc_blk*1e4 <= 1.5) ~ "Mid",
                                       (X2018_inc_blk*1e4 > 1.5) ~ "High"),
                             levels = c("Low","Mid","High"))) -> v_shp_vl_pred_df

v_shp_vl_pred_df %>% 
  group_by(block_endm) %>% 
  group_split() -> by_endm

sapply(by_endm, nrow)
# 37354  5321  2119

get_all_errors <- function(dat){
  bind_rows(
    apply(dplyr::select(dat, pred_inc_bl:pred_inc_rf), 2, 
          function(pred) get_errors2(dat, pred, type = "main"))) %>% 
    mutate(Model = c("Baseline",
                     "Disaggregation (IID)",
                     "Disaggregation (full)",
                     "Random forest"),
           group = unique(dat$block_endm))
}
errors_all_byendm <- bind_rows(lapply(by_endm, get_all_errors)) %>% 
  mutate(Model = factor(Model, levels = c("Baseline",
                                          "Disaggregation (IID)",
                                          "Disaggregation (full)",
                                          "Random forest")))

errors_all_byendm %>% 
  ggplot(aes(group, rmse, col = Model)) +
  geom_point(position = position_dodge(width = 0.2)) +
  scale_colour_manual(values = pal) +
  labs(x = "Block endemicity", y = "RMSE") -> rmse_byendm

errors_all_byendm %>% 
  ggplot(aes(group, ppv, col = Model)) +
  geom_point(position = position_dodge(width = 0.2)) +
  scale_colour_manual(values = pal) +
  labs(x = "Block endemicity", y = "PPV") -> ppv_byendm

errors_all_byendm %>% 
  ggplot(aes(group, rho, ymin = rho_lo, ymax = rho_hi, col = Model)) +
  geom_hline(yintercept = 0, col = "grey",lty = "dashed") +
  geom_errorbar(position = position_dodge(width = 0.3), width = 0.2) +
  geom_point(position = position_dodge(width = 0.3)) + 
  guides(col = "none") +
  scale_colour_manual(values = pal) +
  labs(x = "Block endemicity", y = expression("Spearman's " *rho)) -> rho_byendm

rho_byendm + rmse_byendm + plot_annotation(tag_levels = "a")
ggsave(here::here(figdir,"model_compare_byendm.png"), height = 4, width = 10)

# ---------------------------------------------------------------------------- #
# Plot all predictions

make_plot <- function(obs, pred, title = NULL, rho = NULL, xlims = NULL){
  
  p1 <- plot_scatter(obs, pred, rho = rho, title = title, xlims = xlims)
  p2 <- plot_box(obs, pred, xlims = xlims)
  
  p <- p1 + p2
  return(p)
}

# Check range for axis limits
paste(round(range(
  v_shp_vl_pred %>%
    pivot_longer(pred_inc_bl:pred_inc_rf) %>% 
    pull(value), 
  na.rm = T),
  6),
  collapse = "-")
# "0-739"

# Construct plots for each model
p_bl <- make_plot(v_shp_vl_pred$obs_inc, v_shp_vl_pred$pred_inc_bl, xlims = c(0.00001,750),
                  title = "Baseline", rho = errors_all$rho[errors_all$Model == "Baseline"])
p_dis1 <- make_plot(v_shp_vl_pred$obs_inc, v_shp_vl_pred$pred_inc_iid, xlims = c(0.00001,750),
                    title = "Disaggregation - without field", rho = errors_all$rho[errors_all$Model == "Disaggregation - without field"])
p_dis2 <- make_plot(v_shp_vl_pred$obs_inc, v_shp_vl_pred$pred_inc_full, xlims = c(0.00001,750),
                    title = "Disaggregation - Full", rho = errors_all$rho[errors_all$Model == "Disaggregation - Full"])
p_rf <- make_plot(v_shp_vl_pred$obs_inc, v_shp_vl_pred$pred_inc_rf, xlims = c(0.00001,750),
                  title = "Village-level random forest", rho = errors_all$rho[errors_all$Model == "Village-level random forest"])
p_list <- list(p_bl, p_dis1, p_dis2, p_rf)

# Add axis labels
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
ggsave(here::here(figdir, paste0("vil_obsvpred.png")), p_final, height = 12, width = 10, dpi = 400)
ggsave(here::here("figures/manuscript/Fig6.png"), p_final, height = 12, width = 10, dpi = 400)

# ---------------------------------------------------------------------------- #
# Compare observed versus predicted by category
  
v_shp_vl_pred %>% 
  st_drop_geometry() %>% 
  mutate(pred_bl = pred_inc_bl*replace_na(pop_raster_agg,0)/1000,
         pred_iid = pred_inc_iid*replace_na(pop_raster_agg,0)/1000,
         pred_full = pred_inc_full*replace_na(pop_raster_agg,0)/1000,
         pred_rf = pred_inc_rf*replace_na(pop_raster_agg,0)/1000
  ) %>% 
  dplyr::select(OBJECTID, X2018, pred_bl:pred_rf) %>% 
  pivot_longer(-OBJECTID) %>% 
  mutate(name = factor(name, levels = c("pred_bl",
                                        "pred_iid",
                                        "pred_full",
                                        "pred_rf",
                                        "X2018"), 
                       labels = c("Baseline",
                                  "Disaggregation (without field)",
                                  "Disaggregation (Full)",
                                  "Village-level random forest",
                                  "Observed")),
         value = round(replace_na(value, 0)),
         cat = case_when(value == 0 ~ "0",
                         (value >= 1 & value <=2) ~ "1-2",
                         (value > 2 & value <= 5) ~ "3-5",
                         (value > 5 & value <= 10) ~ "6-10",
                         (value > 10) ~ "> 10"),
         cat = factor(cat, levels = c("0","0-1","1-2","3-5","6-10","> 10"))) %>% 
  group_by(name, value) %>% 
  tally() -> obs_exp_long

write.csv(obs_exp_long, here::here("output","obs_vs_exp_count.csv"), row.names = FALSE)

ggplot(obs_exp_long) +
  geom_col(aes(value, n, fill = name),
           position = position_dodge2(preserve = "single")) +
  labs(fill = "", y = "No. villages", x = "Case count") +
  scale_alpha_manual(values = c("obs" = 1, "pred" = 0.5)) +
  scale_fill_manual(values = c(pal,"darkgrey")) +
  scale_colour_manual(values = c(pal,"darkgrey")) +
  scale_y_continuous(trans = "log2") +
  guides(alpha = "none", col = "none") +
  theme(legend.position = c(0.8,0.8)) -> bycat_all
bycat_all
ggsave(here::here(figdir,"obsvexp_bycats_wpreds.png"), height = 4, width = 8)

# ---------------------------------------------------------------------------- #
# Plot observed and predicted densities

v_shp_vl_pred_df %>% 
  pivot_longer(pred_inc_bl:pred_inc_rf) %>%
  mutate(Model = factor(name, labels = models)) %>% #View()
  ggplot() +
  geom_density(aes(x = value, fill = Model, col = Model), 
               alpha = 0.5) +
  geom_density(data = v_shp_vl_pred, 
               aes(x = obs_inc), 
               lty = "dashed", col = "darkgrey", lwd = 0.7) +
  scale_x_continuous(trans = "log10", limits = c(0.000001,750)) + 
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  theme(legend.position = c(0.2,0.8)) +
  labs(x = "Incidence per 1,000", 
       y = "Density", 
       caption = "Observed density illustrated by dashed line.") -> compare_dens
compare_dens
ggsave(here::here(figdir, paste0("obs_pred_densities.png")), compare_dens, height = 6, width = 8)

# ---------------------------------------------------------------------------- #
# Combine model comparison plots for MS

compare_dens_noleg <- compare_dens + theme(legend.position = "none")
bycat_all / compare_dens_noleg + plot_annotation(tag_levels = "a")
ggsave(here::here(figdir,"model_comparison.png"), height = 8, width = 10)
ggsave(here::here("figures/manuscript/Fig7.png"), height = 8, width = 10, dpi = 300)

################################################################################
################################################################################