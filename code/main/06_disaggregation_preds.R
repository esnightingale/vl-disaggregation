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
theme_set(theme_minimal(base_size = 16))

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
ggsave(here::here("figures/manuscript/Fig4a_revised.png"), height = 6, width = 8, dpi = 300)

# ---------------------------------------------------------------------------- #
# Predict from models

# Include block-level IID effects in main predictions
predictions <- lapply(fits, predict, predict_iid = TRUE)
saveRDS(predictions, here::here("output", "disag_predictions.rds"))

# ---------------------------------------------------------------------------- #
# Plot predictions

## Pixel level ##

plot_preds <- function(pred, 
                       nm, 
                       type = "prediction", #"field" "iid" "covariates"
                       trans = "identity",
                       save = TRUE){
  
  if(is.null(pred$mean_prediction[[type]])){   
    print(paste0(nm,": Type ",type," is Null."))
    return()
  }
  
  pred_rast <- raster::mask(pred$mean_prediction[[type]], boundary) # Extract predicted values
  pred_df <- raster::as.data.frame(pred_rast, xy = T) %>% rename(layer = 3)
  
  if(all(is.na(pred_df$layer))){
    print(paste0(nm,": All raster values NA."))
    return()
    }
  
  if(type == "prediction"){
    fill_lab = "Predicted rate"
    pred_df$layer <- pred_df$layer*1e6 # Scale to rate per 100,000
  }else if(type == "field"){fill_lab = "Fitted field"
  }else if (type == "iid"){fill_lab = "Fitted IID\nper block"
  }else if (type == "covariates"){fill_lab = "Fixed effects"
  }else{fill_lab = NULL}
  
  ggplot(pred_df, aes(x = x, y = y, fill = layer)) +
    geom_raster() +
    scale_fill_viridis_c(option = "rocket", trans = trans, na.value = "transparent") + 
    labs(subtitle = nm,
         x = NULL, y = NULL, 
         fill = fill_lab) +
    theme_void(base_size = 14) +
    theme(legend.position = c(0.85,0.9)) -> p
  
  if(save == T){
    ggsave(here::here(figdir,paste0(type,"_",gsub(" ","",nm),".png")),p, width = 8, height = 6, dpi = 300)
  }
  return(p)
  
}

purrr::map2(predictions, names(predictions), plot_preds, type = "prediction", trans = "log10")
purrr::map2(predictions, names(predictions), plot_preds, type = "field")
purrr::map2(predictions, names(predictions), plot_preds, type = "iid")
purrr::map2(predictions, names(predictions), plot_preds, type = "covariates")

#------------------------------------------------------------------------------#
# Combine for MS figure

p_pred <- plot_preds(predictions$Full, nm = "Full", type = "prediction", trans = "log10", save = F) + 
  labs(subtitle = NULL)
p_field <- plot_preds(predictions$Full, nm = "Full", type = "field", save = F) + 
  labs(subtitle = NULL)

# Make final figure
plot_covs + (p_pred / p_field) + plot_annotation(tag_levels = "a") + plot_layout(widths = c(1,1))
ggsave(here::here("figures/manuscript","Fig4_revised.png"), width = 15, height = 9, dpi = 300)

################################################################################
################################################################################