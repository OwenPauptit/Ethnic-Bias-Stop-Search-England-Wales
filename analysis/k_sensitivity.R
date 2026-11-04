## K is the offset for arrests to ensure the model is well defined when the number of arrests is 0
## Here, we test sensitivity to the choice of K
fit.models.with.varying.k <- function(ks, data) {
  models.with.varying.k <- lapply(ks, function(k) fit.single.model(data, k = k))
  return(models.with.varying.k)
}

temporal.plot.varying.k <- function(models.with.varying.k) {
  
  k.lincomb.result <- lapply(models.with.varying.k, function(k_fit) lapply(k_fit$marginals.lincomb.derived,
                                                                           function(m) as.data.frame(inla.zmarginal(inla.tmarginal(exp, m)))))
  
  k.lincomb.df <- do.call(rbind, lapply(1:4,
                                        function(i) do.call(rbind, k.lincomb.result[[i]]) %>%
                                          rownames_to_column("ID") %>%
                                          mutate(
                                            ID = as.integer(substr(ID, 3,4)),
                                            ethnicity = c("Asian", "Black")[as.integer(ID > 8)+1],
                                            year = ((ID-1) %% 8) + 2016,
                                            k = ks[i]
                                          ) ))
  
  p <- k.lincomb.df  %>%
    ggplot(aes(x = year,color = ethnicity, linetype = ethnicity)) + 
    geom_line(aes(y = mean))  +
    geom_errorbar(aes(ymin = quant0.025, ymax=quant0.975),
                  position = position_dodge(width = 0.6)) +
    geom_hline(aes(yintercept = 1, color="White", linetype = "White")) +
    facet_wrap(vars(paste("k =", k)), nrow=1) +
    theme_classic() +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(size = rel(1), color = "black")
    ) +
    scale_color_manual(values = c("black", "black", "red")) +
    scale_x_continuous(breaks = c(2017, 2020)) +
    scale_linetype_manual(values = c("solid", "dotted", "solid")) +
    labs(
      x = "Financial year beginning",
      y = "Posterior Ratio",
      colour = "Ethnicity",
      linetype = "Ethnicity"
    )
  
  return(p)
}

