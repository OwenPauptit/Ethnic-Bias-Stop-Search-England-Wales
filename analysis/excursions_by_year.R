library(stringr)

# Compute the posterior ratio of stop rate for ethnic minorities / stop rate for White people for every combination of police force and financial year
compute.ratios.over.force.and.year <- function(model) {
  
  samples.force.year <- inla.posterior.sample(n = 10000, 
                                              model, 
                                              selection = list(ethnicity = 0,
                                                               `interaction(ethnicity, force)` = 0,
                                                               `interaction(ethnicity, year)` = 0)
  )
  
  samples.force.year.df <- do.call(rbind, lapply(samples.force.year, function(sample) sample$latent %>%
                                                   as.data.frame() %>%
                                                   rownames_to_column("id") %>%
                                                   pivot_wider(names_from = id, values_from = V1))) 
  
  # Complicated reformatting of dataframe
  results.force.year <- samples.force.year.df %>%
    rownames_to_column("iter") %>%
    pivot_longer(starts_with("ethnicity"), names_to = "ethnicity", values_to = "eth_val") %>% 
    pivot_longer(starts_with("interaction(ethnicity, year"), names_to = "ethyear", values_to = "ethyear_val") %>%
    mutate(ethnicity =as.integer(str_extract(ethnicity, "\\d")),
           ethyear = as.integer(str_extract(ethyear, "\\d+"))) %>%
    filter(
      ethnicity == (ethyear - 1) %% 3 + 1
    ) %>%
    pivot_longer(starts_with("interaction(ethnicity, force"), names_to = "ethforce", values_to = "ethforce_val") %>%
    mutate(
      ethforce = as.integer(str_extract(ethforce, "\\d+"))) %>%
    filter(
      ethnicity == (ethforce - 1) %% 3 + 1
    ) %>%
    mutate(
      rate = eth_val + ethyear_val + ethforce_val,
      ethnicity = c("Asian", "Black", "White")[ethnicity],
      year = (ethyear - 1) %/% 3 + 1 + 2015,
      force = levels(data$force)[(ethforce - 1) %/% 3 + 1]
    ) %>%
    group_by(iter, force, year) %>%
    summarise(asian_log_ratio = rate[ethnicity == "Asian"] - rate[ethnicity == "White"],
              black_log_ratio = rate[ethnicity == "Black"] - rate[ethnicity == "White"]) 
  
  return(results.force.year)
}

## Compute and plot excursion sets for each financial year
compute.asian.excursion.sets.by.year <- function(ratios.by.force.and.year) {
  asian_ratios_by_year <- lapply(2016:2023,
                                 function(yr) ratios.by.force.and.year %>% 
                                   filter(year == yr) %>%
                                   select(-black_log_ratio, -year) %>%
                                   pivot_wider(names_from = iter, values_from = asian_log_ratio) %>%
                                   column_to_rownames("force"))
  
  asian_exsets_by_year <- lapply(asian_ratios_by_year,
                                 function(mcsamples)
                                   excursions.mc(
                                     mcsamples,
                                     alpha = 0.05,
                                     u = 0,
                                     type = ">"
                                   )$E)
  
  exset_df <- as.data.frame(do.call(cbind, asian_exsets_by_year))
  colnames(exset_df) <- paste0(2016:2023, "/", 17:24)
  rownames(exset_df) <- rownames(asian_ratios_by_year[[1]])
  
  exset_df_long <- exset_df %>%
    rownames_to_column("police_force") %>%
    pivot_longer(-police_force, names_to = "year", values_to = "excursion")
  
  return(exset_df_long)
}

plot.excursion.set.by.year <- function(excursion.sets.by.year, areas) {
  exset_yr_area <- full_join(areas, excursion.sets.by.year)
  
  p <- ggplot(exset_yr_area) + 
    geom_sf(aes(fill = as.logical(excursion))) +
    facet_wrap(vars(year), nrow=2) +
    theme_classic() + 
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(size = rel(1.0), colour = "black")) +
    scale_fill_manual(values = c("white", "red"))
  
  return(p)
}


compute.black.excursion.sets.by.year <- function(ratios.by.force.and.year) {
  black_ratios_by_year <- lapply(2016:2023,
                                 function(yr) ratios.by.force.and.year %>% 
                                   filter(year == yr) %>%
                                   select(-asian_log_ratio, -year) %>%
                                   pivot_wider(names_from = iter, values_from = black_log_ratio) %>%
                                   column_to_rownames("force"))
  
  black_exsets_by_year <- lapply(black_ratios_by_year,
                                 function(mcsamples)
                                   excursions.mc(
                                     mcsamples,
                                     alpha = 0.05,
                                     u = 0,
                                     type = ">"
                                   )$E)
  
  exset_df <- as.data.frame(do.call(cbind, black_exsets_by_year))
  colnames(exset_df) <- paste0(2016:2023, "/", 17:24)
  rownames(exset_df) <- rownames(black_ratios_by_year[[1]])
  
  exset_df_long <- exset_df %>%
    rownames_to_column("police_force") %>%
    pivot_longer(-police_force, names_to = "year", values_to = "excursion")
  
  return(exset_df_long)
}
