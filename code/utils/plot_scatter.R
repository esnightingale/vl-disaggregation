plot_scatter <- function(obs, pred, rho, xlab = NULL, ylab = NULL, title = NULL,
                         xlims =  c(0.0001,1000)){
  data.frame(obs = obs, pred = pred) %>% 
    filter(obs > 0 & pred > 0) %>% 
    ggplot(aes(pred, obs)) +
    geom_point(alpha = 0.1) + 
    geom_smooth(method = "lm", col = "grey") +
    scale_x_continuous(trans = "log10", limits = xlims) +
    scale_y_continuous(trans = "log10") +
    labs(y = ylab, x = xlab,
         title = title,
         caption = paste0("Rho = ",round(rho,2))) -> p
  return(p)
}
