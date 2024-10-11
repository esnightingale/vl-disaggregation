plot_box <- function(obs, pred, xlab = NULL, ylab = NULL, title = NULL,
                     xlims = NULL){
  data.frame(obs = obs, pred = pred) %>%
    filter(!is.na(obs)) %>% 
    mutate(y = factor(obs > 0, levels = c(FALSE, TRUE), labels = c("Unaffected","Affected"))) %>% 
    ggplot(aes(y = y, x = pred)) +
    geom_boxplot() +
    scale_x_continuous(trans = "log10", limits = xlims) +
    labs(y = ylab, x = xlab, title = title) -> p
  return(p)
}