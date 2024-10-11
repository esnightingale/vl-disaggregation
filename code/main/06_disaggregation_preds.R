################################################################################
################################################################################

library(disaggregation)
library(raster)
library(tidyverse)
library(sf)
library(patchwork)
library(cowplot)

# Fix default ggsave background
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
theme_set(theme_minimal())

# Local data folder
datadir <- "C:/Users/phpuenig/Documents/VL/Data"
figdir <- "figures/disaggregation/predictions"

# State boundary for plotting
boundary <- readRDS(here::here("data","analysis","boundary.rds")) %>% 
  st_as_sf() %>% 
  st_transform(4326) 

# Model fits
fits <- readRDS(here::here("output","disag_fits.rds"))

# ---------------------------------------------------------------------------- #
# Summary function

get_covs <- function(fit,nm){
  
  covnames <- names(fit$data$covariate_rasters)
  
  covtab <- summary(fit)[["model_params"]] %>% 
    as.data.frame() %>% 
    head(length(covnames)+1) %>% 
    mutate(Model = nm,
           Parameter = c("Intercept", covnames),
           conf.low = Estimate - 1.96*`Std. Error`,
           conf.high = Estimate + 1.96*`Std. Error`) 
  rownames(covtab) <- NULL
  return(covtab)
}

################################################################################

# Summarise parameter estimates

covtab <- bind_rows(purrr::map2(fits, names(fits), get_covs))
write.csv(covtab, here::here("output","disag_fixeff_raw.csv"), row.names = F)

covtabtidy <- covtab %>% 
  mutate(Value = paste0(round(Estimate, 2), " (",
                        round(`Std. Error`, 3),")")) %>% 
  dplyr::select(Model, Parameter, Value) %>% #
  tidyr::pivot_wider(values_from = "Value", names_from= "Model")
write.csv(covtabtidy, here::here("output","disag_fixeff_tidy.csv"), row.names = F)

# ---------------------------------------------------------------------------- #
# Plot parameter estimates

covnames <- c("Intercept", "Elevation","Distance to water body",
              "Travel time to city","LST (annual mean)",
              "LST (annual SD)","NDVI (annual mean)","NDVI (annual SD)")
covtab %>% 
  mutate(Parameter = factor(Parameter, 
                            levels = rev(unique(covtab$Parameter)),
                            labels = rev(covnames)),
         Model = factor(Model, levels = c("IID only","Full"), labels = c("Without\nspatial field","With\nspatial field"))) %>% 
  filter(Parameter != "Intercept") %>% 
  ggplot(aes(x = Parameter, y = Estimate, ymin = conf.low, ymax = conf.high, col = Model)) +
  geom_hline(yintercept = 0, col = "grey", lty = "dashed") +
  geom_errorbar(position = position_dodge(w = 0.5), width = 0.4) +
  geom_point(position = position_dodge(w = 0.5)) +
  coord_flip() +
  labs(x = "", col = "") -> plot_covs

plot_covs
ggsave(here::here(figdir, "disag_fixeff.png"), height = 6, width = 8)
ggsave(here::here("figures/manuscript/Fig4a.png"), height = 6, width = 8)

# ---------------------------------------------------------------------------- #
# Predict from models

# Include block-level IID effects in main predictions
predictions <- lapply(fits, predict, predict_iid = TRUE)
saveRDS(predictions, here::here("output", "disag_predictions.rds"))

# ---------------------------------------------------------------------------- #
# Plot predictions

## Pixel level ##
plot_preds <- function(pred, nm){
  png(here::here(figdir,paste0("pred_",gsub(" ","",nm),".png")), width = 800, height = 600)
  plot(mask(pred$mean_prediction$prediction, boundary))
  dev.off()
}

purrr::map2(predictions, names(predictions), plot_preds)

plot_pred_iid <- function(pred, nm){
  png(here::here(figdir,paste0("pred_iid_",gsub(" ","",nm),".png")), width = 800, height = 600)
  plot(mask(pred$mean_prediction$iid, boundary))
  dev.off()
}
purrr::map2(predictions, names(predictions), plot_pred_iid)

plot_pred_cov <- function(pred, nm){
  png(here::here(figdir,paste0("pred_cov_",gsub(" ","",nm),".png")), width = 800, height = 600)
  plot(mask(pred$mean_prediction$covariates, boundary))
  dev.off()
}
purrr::map2(predictions, names(predictions), plot_pred_cov)


# Map fitted spatial field 
pred_final <- predictions$Full
png(here::here(figdir,"fitted_field.png"), width = 800, height = 600)
plot(mask(pred_final$mean_prediction$field, boundary))
dev.off()

#------------------------------------------------------------------------------#
# Combine for MS figure

png(here::here("figures/manuscript/Fig4b.png"), width = 800, height = 1200)
par(mfrow = c(2,1))
plot(mask(pred_final$mean_prediction$prediction, boundary))
plot(mask(pred_final$mean_prediction$field, boundary))
dev.off()

# plot(mask(pred_final$mean_prediction$prediction, boundary))
# fig4b1 <- recordPlot()
# plot(mask(pred_final$mean_prediction$field, boundary))
# fig4b2 <- recordPlot()
# 
# fig4b <- plot_grid(fig4b1, fig4b2,
#           ncol = 1)
# 
# plot_grid(plot_covs, fig4b,
#           ncol = 2)
# ggsave(here::here("figures/manuscript/Fig4_test.png"), height = 6, width = 15)

################################################################################
################################################################################