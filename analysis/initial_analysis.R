plot.temporal <- function(model) {
  lincomb.result <- lapply(model$marginals.lincomb.derived,
                           function(m) as.data.frame(inla.zmarginal(inla.tmarginal(exp, m))))
  
  p <- do.call(rbind, lincomb.result) %>%
    rownames_to_column("ID") %>%
    mutate(
      ID = as.integer(substr(ID, 3,4)),
      year = ((ID-1) %% 8) + 2016,
      ethnicity = c("Asian", "Black", "White")[(ID - 1) %/% 8 + 1]
    ) %>%
    ggplot(aes(x = year,linetype = ethnicity, colour = ethnicity)) + 
    geom_line(aes(y = mean))  +
    geom_errorbar(aes(ymin = quant0.025, ymax=quant0.975),
                  position = position_dodge(width = 0.6)) +
    geom_hline(aes(yintercept = 1, colour = "White", linetype = "White")) +
    theme_classic() +
    scale_color_manual(values = c("black", "black", "red")) +
    scale_x_continuous(breaks = c(2017, 2020)) +
    scale_linetype_manual(values = c("solid", "dotted", "solid")) +
    labs(
      x = "Financial year beginning",
      y = "Posterior ratio of stops per arrest",
      linetype = "Ethnicity",
      colour = "Ethnicity"
    )
  
  return(p)
}

# Overall Excursion sets 

compute.overall.excursion.sets <- function(model, data ) {
  
  print("Taking posterior samples...")
  samples <- inla.posterior.sample(n = 10000, 
                                   model, 
                                   selection = list(ethnicity = 0,
                                                    `interaction(ethnicity, force)` = 0)
  )
  
  compute.ratio.of.sample <- function(sample) {
    df <- sample$latent %>%
      as.data.frame() %>%
      rownames_to_column("id") %>%
      rename(value = V1)
    
    eth <- df %>% 
      filter(startsWith(id, "eth")) %>%
      select(-id)
    
    rownames(eth) = levels(data$ethnicity)
    
    int <- df %>% filter(startsWith(id, "int")) %>%
      mutate(name = levels(interaction(data$ethnicity, data$force)),
             ethnicity = substr(name, 1, 5),
             force = substr(name, 7, 50)) %>%
      select(-name, -id) %>%
      pivot_wider(names_from = ethnicity, values_from = value) %>%
      mutate(asian_log_ratio = asian - white + eth["asian",] - eth["white",],
             black_log_ratio = black - white+ eth["asian",] - eth["white",]) %>%
      select(force, asian_log_ratio, black_log_ratio)
    
    return(int)
  }
  
  print("Computing ratios of rates...")
  ratios <- lapply(samples, compute.ratio.of.sample)
  
  asian.ratios <- do.call(cbind,
                          lapply(ratios, 
                                 function(df) df %>% 
                                   column_to_rownames("force") %>% 
                                   select(asian_log_ratio)))
  
  
  black.ratios <- do.call(cbind,
                          lapply(ratios, 
                                 function(df) df %>% 
                                   column_to_rownames("force") %>% 
                                   select(black_log_ratio)))
  
  print("Computing Asian excursion set...")
  asian.exset <- excursions.mc(
    asian.ratios,
    alpha = 0.05,
    u = 0,
    type = "!="
  )
  
  
  
  print("Computing Black excursion set...")
  black.exset <- excursions.mc(
    black.ratios,
    alpha = 0.05,
    u = 0,
    type = "!="
  )
  
  return(list(asian.upr = rownames(asian.ratios)[asian.exset$M == 1],
              asian.lwr = rownames(asian.ratios)[asian.exset$M == 0],
              black.upr = rownames(asian.ratios)[black.exset$M == 1],
              black.lwr = rownames(asian.ratios)[black.exset$M == 0],
              asian.F = asian.exset$F,
              black.F = black.exset$F,
              asian.ratios = asian.ratios,
              black.ratios = black.ratios))
}

plot.overall.excursion.sets <- function(excursion.sets, areas) {
  
  p1 <- areas %>%
    mutate(
      excursion.upr = police_force %in% excursion.sets$asian.upr,
      excursion.lwr = police_force %in% excursion.sets$asian.lwr,
      excursion = as.factor(excursion.upr - excursion.lwr)
    ) %>%
    ggplot() +
    geom_sf(aes(fill = excursion)) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(size = rel(1.0), colour = "black")) +
    scale_fill_manual(values = c("blue", "white", "red"), breaks = -1:1) +
    ggtitle("Asian")
  
  p2 <- areas %>%
    mutate(
      excursion.upr = police_force %in% excursion.sets$black.upr,
      excursion.lwr = police_force %in% excursion.sets$black.lwr,
      excursion = as.factor(excursion.upr - excursion.lwr)
    ) %>%
    ggplot() +
    geom_sf(aes(fill = excursion)) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(size = rel(1.0), colour = "black")) +
    scale_fill_manual(values = c("blue", "white", "red"), breaks = -1:1) +
    ggtitle("Black")
  
  return(p1+p2)
} 


