inc_to_abs <- function(pred_rast){
  return(
    raster::overlay(pred_rast,
            population_raster,
            fun = function(x,y){return(x*y)})
  )
}

get_cases_mean <- function(pred){
  return(
    inc_to_abs(pred$mean_prediction$prediction)
  )
}

get_cases_ci <- function(pred){
  return(
    inc_to_abs(pred$uncertainty_prediction$predictions_ci)
  )
}

get_cases_sims <- function(pred){
  return(
    inc_to_abs(pred$uncertainty_prediction$realisations)
  )
}
