
library(stringr)

set.seed(9231864)

# Omit model fitting for now

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

###############

# TEMPORAL PLUS EXCURSION SET (ASIAN)

summarised.results.force.year <- results.force.year %>%
  group_by(force, year) %>%
  mutate(asian = exp(asian_log_ratio),
         black = exp(black_log_ratio)) %>%
  summarise(
    asian_LB = quantile(asian, 0.025),
    asian_mean = mean(asian),
    asian_UB = quantile(asian, 0.975),
    black_LB = quantile(black, 0.025),
    black_mean = mean(black),
    black_UB = quantile(black, 0.975)
  )

p1 <- summarised.results.force.year %>%
  filter(force %in% c("lancashire", "north_yorkshire", "west_midlands"))%>%
  ggplot(aes(x = year, y = asian_mean)) + 
  geom_line() + 
  geom_errorbar(aes(ymin = asian_LB, ymax = asian_UB)) + 
  geom_hline(yintercept = 1, colour = "red") +
  facet_wrap(vars( str_to_title(gsub("_", " ",force))), nrow=3) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = rel(1.0), colour = "black")
  ) +
  labs(
    x = "Financial year",
    y = "Posterior ratio"
  )

p2 <- areas %>%
  mutate(
    excursion = police_force %in% c("lancashire", "north_yorkshire", "west_midlands")
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
  scale_fill_manual(values = c("white", "red"))

p1 + p2

ggsave("visualisations/posterior_ratio_exset_and_temporal.pdf", width=6, height=3, units="in")

##############

# Temporal Plus excursion (black)

p1 <- summarised.results.force.year %>%
  filter(force %in% c("lancashire"))%>%
  ggplot(aes(x = year, y = black_mean)) + 
  geom_line() + 
  geom_errorbar(aes(ymin = black_LB, ymax = black_UB)) + 
  geom_hline(yintercept = 1, colour = "red") +
  facet_wrap(vars( str_to_title(gsub("_", " ",force))), nrow=3) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = rel(1.0), colour = "black")
  ) +
  labs(
    x = "Financial year",
    y = "Posterior ratio"
  )

p2 <- areas %>%
  mutate(
    excursion = police_force %in% c("lancashire")
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
  scale_fill_manual(values = c("white", "red"))

p1 + p2

ggsave("visualisations/posterior_ratio_exset_and_temporal_black.pdf", width=6, height=3, units="in")

############

# Exsets by year (ASian)

asian_ratios_by_year <- lapply(2016:2023,
                               function(yr) results.force.year %>% 
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

exset_yr_area <- full_join(areas, exset_df_long)

ggplot(exset_yr_area) + 
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

ggsave("visualisations/excursions_by_year_asian.pdf", width=6, height=3, units="in")

########
# Exsets by year (Black)

black_ratios_by_year <- lapply(2016:2023,
                               function(yr) results.force.year %>% 
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
rownames(exset_df) <- rownames(asian_ratios_by_year[[1]])

exset_df_long <- exset_df %>%
  rownames_to_column("police_force") %>%
  pivot_longer(-police_force, names_to = "year", values_to = "excursion")

exset_yr_area <- full_join(areas, 
                           exset_df_long)

ggplot(exset_yr_area) + 
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

ggsave("visualisations/excursions_by_year_black.pdf", width=6, height=3, units="in")