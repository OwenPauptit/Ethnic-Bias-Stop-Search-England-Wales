library(fBasics)
library(patchwork)

compute.replicates <- function(model, data) {
  samples <- inla.posterior.sample(n = 10000, model, selection = list(Predictor = 0))
  
  samples.df <- do.call(rbind, lapply(samples, function(sample) sample$latent[,1]))
  
  reps <- matrix(nrow = nrow(samples.df), ncol = ncol(samples.df))
  for (i in 1:ncol(reps)) {
    reps[,i] <- rpois(nrow(reps), exp(samples.df[, i]))
  }
  
  return(reps)
}

plot.posterior.predictive.check <- function(replicates) {
  
  post.pred <- as.data.frame(reps) %>%
  rownames_to_column("iter") %>%
  pivot_longer(-iter, names_to = "y_rep_idx", values_to = "y_rep") %>%
  group_by(iter) %>%
  summarise(
    mean = mean(y_rep),
    sd = sd(y_rep),
    min = min(y_rep),
    max = max(y_rep),
    median = median(y_rep),
    skewness = skewness(y_rep),
    kurtosis = kurtosis(y_rep)
  )

  p1 <- post.pred %>%
    ggplot() +
    geom_histogram(aes(x = sd, colour = "Replicates")) +
    geom_vline(aes(xintercept = sd(data$stops), colour = "Real")) +
    labs(
      x = "Std. deviation"
    ) 
  
  p2 <- post.pred %>%
    ggplot() +
    geom_histogram(aes(x = max, colour = "Replicates")) +
    geom_vline(aes(xintercept = max(data$stops), colour = "Real")) +
    labs(
      x = "Maximum"
    ) 
  
  p3 <- post.pred %>%
    ggplot() +
    geom_histogram(aes(x = kurtosis, colour = "Replicates")) +
    geom_vline(aes(xintercept = kurtosis(data$stops), colour = "Real")) +
    labs(
      x = "Kurtosis"
    ) 
  
  
  p4 <- post.pred %>%
    ggplot() +
    geom_histogram(aes(x = skewness, colour = "Replicates")) +
    geom_vline(aes(xintercept = skewness(data$stops), colour = "Real")) +
    labs(
      x = "Skewness"
    ) +
    scale_x_continuous(breaks = c(11.1, 11.2, 11.3))
  
  
  return(p1 + p2 + p3 + p4 + plot_layout(guides = "collect", nrow = 2) &
    scale_colour_manual(values = c("red","#555555"), name = "Data") &
    theme_classic())
}


