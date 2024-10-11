get_errors <- function(obs, pred){
  data.frame(rho = cor(obs, pred, method = "spearman", use = "complete.obs"),
              rmse = sqrt(mean((obs - pred)^2, na.rm = T))) %>% 
    return()
}

# https://stats.stackexchange.com/questions/18887/how-to-calculate-a-confidence-interval-for-spearmans-rank-correlation
spearman_CI <- function(x, y, alpha = 0.05){
  rs <- cor(x, y, method = "spearman", use = "complete.obs")
  n <- sum(complete.cases(x, y))
  ci <- sort(tanh(atanh(rs) + c(-1,1)*sqrt((1+rs^2/2)/(n-3))*qnorm(p = alpha/2))) 
  return(c(rho = rs, rho_lo = ci[1], rho_hi = ci[2]))
}

get_errors2 <- function(df, preds, type = "main"){
  
  if (type == "main"){
    df <- rename(df, pop = pop_raster_agg)
  }else if (type == "sens"){
    df <- rename(df, pop = pop_care_estimate)
  }
 # browser()
  df %>% 
    mutate(pred_inc = unlist(preds),
           pred_cases = pred_inc*na_if(pop,0)/1000) %>% 
    summarise(rho_ci = paste0(spearman_CI(obs_inc, pred_inc), collapse = "//"),
              rmse = sqrt(mean((obs_inc - pred_inc)^2, na.rm = T)),
              mae = mean(abs(obs_inc - pred_inc), na.rm = T),
              perc_agree_affect = 100*mean((X2018 > 0) == (round(pred_cases,0) > 0), na.rm = T),
              sens = mean(pred_inc[obs_inc > 0] > 0, na.rm = T),
              spec = mean(obs_inc[pred_inc > 0] > 0, na.rm = T),
              ppv = sum(pred_inc[obs_inc > 0] > 0, na.rm = T)/sum(pred_inc > 0, na.rm = T)) %>% 
    separate(rho_ci, into = c("rho","rho_lo","rho_hi"), sep = "//", remove = T, convert = T) %>% 
    mutate(rho_ci = paste0(round(rho,2), " [",
                           round(rho_lo,2), ", ",
                           round(rho_hi,2),"]")) %>% 
    return()
}
