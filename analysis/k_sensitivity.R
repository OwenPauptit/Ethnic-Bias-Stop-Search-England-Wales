
source("fit_single_model.R")


fit.models.with.varying.k <- function(ks, data) {
  models.with.varying.k <- lapply(ks, function(k) inla(stops ~ offset(log(arrests + 0.01)) +
                                          f(ethnicity, constr=TRUE) + 
                                          f(force, constr=TRUE) + 
                                          f(year, model = "ar1", constr=TRUE) +
                                          f(crime, constr=TRUE) + 
                                          f(interaction(ethnicity,year), constr=TRUE) +
                                          f(interaction(ethnicity, force), constr=TRUE) +
                                          f(interaction(force, year), constr=TRUE) +
                                          f(interaction(crime, ethnicity), constr=TRUE) + 
                                          f(interaction(crime, year), constr=TRUE) +
                                          f(interaction(crime, force), constr=TRUE) +
                                          f(obs, constr=TRUE),
                                        family = "poisson",
                                        data = data,
                                        lincomb = get.eth.year.lincomb(data),
                                        control.compute = list(config = TRUE),
                                        control.predictor = list(compute = TRUE)))
  
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



###########

# K excursions
compute.excursion.sets.with.varying.k <- function(models.with.varying.k, data) {
  k.samples <- lapply(models.with.varying.k,
                      function(k_fit) inla.posterior.sample(n = 10000, 
                                                            k_fit, 
                                                            selection = list(ethnicity = 0,
                                                                             `interaction(ethnicity, force)` = 0)))
  
  k.samples.df <- lapply(k.samples,
                         function(k.sample) do.call(rbind, lapply(k.sample, function(sample) sample$latent %>%
                                                                    as.data.frame() %>%
                                                                    rownames_to_column("id") %>%
                                                                    pivot_wider(names_from = id, values_from = V1))))
  
  
  k.results <- lapply(k.samples.df, function(k.sample.df) k.sample.df %>%
                        rownames_to_column("iter") %>%
                        pivot_longer(starts_with("ethnicity"), names_to = "ethnicity", values_to = "eth_val") %>% 
                        pivot_longer(starts_with("interaction(ethnicity, force"), names_to = "ethforce", values_to = "ethforce_val") %>%
                        mutate(ethnicity =as.integer(str_extract(ethnicity, "\\d")),
                               ethforce = as.integer(str_extract(ethforce, "\\d+"))) %>%
                        filter(
                          ethnicity == (ethforce - 1) %% 3 + 1
                        ) %>%
                        mutate(
                          rate = eth_val +  ethforce_val,
                          ethnicity = c("Asian", "Black", "White")[ethnicity],
                          force = levels(data$force)[(ethforce - 1) %/% 3 + 1]
                        ) %>%
                        group_by(iter, force) %>%
                        summarise(asian_log_ratio = rate[ethnicity == "Asian"] - rate[ethnicity == "White"],
                                  black_log_ratio = rate[ethnicity == "Black"] - rate[ethnicity == "White"]) )
  
  k.ready.for.excursion.asian <- lapply(k.results, function(k.result) 
    k.result %>% 
      select(-black_log_ratio) %>%
      mutate(iter = as.integer(iter)) %>%
      arrange(iter) %>%
      pivot_wider(names_from = iter, values_from = asian_log_ratio) %>%
      column_to_rownames("force")
  )
  
  k.excursion.asian <- lapply(k.ready.for.excursion.asian, function(df)
    rownames(df)[as.logical((df %>%
                               excursions.mc(
                                 alpha = 0.05,
                                 u = 0,
                                 type = ">"
                               ))$E)])
  
  return(k.excursion.asian)
}

