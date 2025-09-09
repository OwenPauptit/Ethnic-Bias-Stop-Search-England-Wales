library(tidyverse)
library(INLA)
library(excursions)

set.seed(42164)

data <- read_csv("data/cleaned/stopsearch_arrests.csv") %>%
  rownames_to_column("obs") %>%
  mutate(
    force = factor(police_force),
    ethnicity = factor(officer_defined_ethnicity),
    crime = factor(crime_type),
    year = factor(previous_financial_year),
    stops = number_of_stops,
    arrests = number_of_arrests,
    obs = factor(obs)
  ) %>%
  select(ethnicity, force, crime, year, stops, arrests, obs)

##### Prepare linear combinations

# Code to create weights to compute ratios (beta_E + beta_E,T - beta_White - beta_White,T)
int.diag.no.zeros <- function(x) {
  M <- diag(as.integer(x))
  return(M[rowSums(M) > 0,])
}

get.weights.for.ethnicity <- function(data, ethnicity) {
  return(
    int.diag.no.zeros(
      startsWith(
        levels(interaction(data$ethnicity, data$year)), 
        ethnicity)))
}

weights.eth.year.asian <- get.weights.for.ethnicity(data, "asian")
weights.eth.year.black <- get.weights.for.ethnicity(data, "black")
weights.eth.year.white <- get.weights.for.ethnicity(data, "white")

weights.eth.year.asian.white <- weights.eth.year.asian - weights.eth.year.white
weights.eth.year.black.white <- weights.eth.year.black - weights.eth.year.white

weights.eth.asian.white <- t(matrix(rep(c(1, NA, -1), 
                                        nrow(weights.eth.year.asian.white)), 
                                    nrow = 3))
weights.eth.black.white <- t(matrix(rep(c(NA, 1, -1), 
                                        nrow(weights.eth.year.asian.white)), 
                                    nrow = 3))

weights.eth.year <- rbind(weights.eth.year.asian.white, 
                          weights.eth.year.black.white)
weights.eth <- rbind(weights.eth.asian.white, 
                     weights.eth.black.white)

lin.combs.eth.year <- inla.make.lincombs(
  ethnicity = weights.eth,
  `interaction(ethnicity, year)` = weights.eth.year
)

######

# Create weights to compute rate: mu + beta_E + beta_E,T
weights.eth.year.all <- rbind(weights.eth.year.asian,
                              weights.eth.year.black,
                              weights.eth.year.white)

weights.eth.asian <- t(matrix(rep(c(1,0,0), nrow(weights.eth.year.asian)), nrow=3))
weights.eth.black <- t(matrix(rep(c(0,1,0), nrow(weights.eth.year.asian)), nrow=3))
weights.eth.white <- t(matrix(rep(c(0,0,1), nrow(weights.eth.year.asian)), nrow=3))

weights.eth.all <- rbind(weights.eth.asian,
                         weights.eth.black,
                         weights.eth.white)

weights.year <- diag(1, nrow=8)
weights.year.all <- rbind(weights.year, 
                          weights.year, 
                          weights.year)

weights.intercept <- as.matrix(rep(1, nrow(weights.eth.all)), nrow=nrow(weights.year.all))

lincomb.all <- inla.make.lincombs(
  `(Intercept)` = weights.intercept,
  ethnicity = weights.eth.all,
  year = weights.year.all,
  `interaction(ethnicity, year)` = weights.eth.year.all
)

##########

# Fit to produce rates

fit <- inla(stops ~ offset(log(arrests + 0.01)) +
              f(ethnicity, constr=TRUE) + 
              f(force, constr=TRUE) + 
              f(year, constr=TRUE) +
              f(crime, constr=TRUE) + 
              f(interaction(ethnicity,year), constr=TRUE) +
              f(interaction(ethnicity, force), constr=TRUE) +
              f(interaction(force, year), constr=TRUE) +
              f(obs, constr=TRUE),
            family = "poisson",
            data = data,
            lincomb = lincomb.all,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE))

lincomb.result <- lapply(fit$marginals.lincomb.derived,
                         function(m) as.data.frame(inla.zmarginal(inla.tmarginal(exp, m))))

do.call(rbind, lincomb.result) %>%
  rownames_to_column("ID") %>%
  mutate(
    ID = as.integer(substr(ID, 3,4)),
    year = ((ID-1) %% 8) + 2016,
    ethnicity = c("Asian", "Black", "White")[(ID - 1) %/% 8 + 1]
  ) %>%
  ggplot(aes(x = year,linetype = ethnicity)) + 
  geom_line(aes(y = mean))  +
  geom_errorbar(aes(ymin = quant0.025, ymax=quant0.975),
                position = position_dodge(width = 0.6)) +
  theme_classic() +
  labs(
    x = "Financial year beginning",
    y = "Posterior rate of stops per arrest",
    linetype = "Ethnicity"
  )

ggsave("visualisations/posterior_rate_k_0_01.pdf", width=6, height=3, units="in")

##############

# Fit to produce ratios (used for everything else)

fit <- inla(stops ~ offset(log(arrests + 0.01)) +
              f(ethnicity, constr=TRUE) + 
              f(force, constr=TRUE) + 
              f(year, constr=TRUE) +
              f(crime, constr=TRUE) + 
              f(interaction(ethnicity,year), constr=TRUE) +
              f(interaction(ethnicity, force), constr=TRUE) +
              f(interaction(force, year), constr=TRUE) +
              # f(interaction(ethnicity, crime), constr=TRUE) +
              # f(interaction(crime, year), constr=TRUE) +
              # f(interaction(crime, force), constr=TRUE) +
              f(obs, constr=TRUE),
            family = "poisson",
            data = data,
            lincomb = lin.combs.eth.year,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE)
)

# Compute summary stats for std dev of each random effect: alter `Precision for ...`
inla.zmarginal(inla.tmarginal(function(tau) 1/sqrt(tau),
               fit$marginals.hyperpar$`Precision for ethnicity`
))

# Summary stats for ethnicity
fit$summary.random$ethnicity

# Summary stats for mu
fit$summary.fixed

###########

# Overall Excursion sets 

samples <- inla.posterior.sample(n = 10000, 
                                 fit, 
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

exset <- excursions.mc(
  asian.ratios,
  alpha = 0.05,
  u = 0,
  type = ">"
)

asian.forces.exset <- rownames(asian.ratios)[as.logical(exset$E)]

black.exset <- excursions.mc(
  black.ratios,
  alpha = 0.05,
  u = 0,
  type = ">"
)

black.forces.exset <- rownames(black.ratios)[as.logical(black.exset$E)]

#################

# BOTH FORCE AND YEAR

samples.force.year <- inla.posterior.sample(n = 10000, 
                                            fit, 
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
  pivot_longer(starts_with("interaction(ethnicity, force"), names_to = "ethforce", values_to = "ethforce_val") %>%
  mutate(ethnicity =as.integer(str_extract(ethnicity, "\\d")),
         ethyear = as.integer(str_extract(ethyear, "\\d+")),
         ethforce = as.integer(str_extract(ethforce, "\\d+"))) %>%
  filter(
    ethnicity == (ethyear - 1) %% 3 + 1,
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

exset_yr_area <- full_join(areas %>% st_simplify(preserveTopology=T, dTolerance=2000), 
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

exset_yr_area <- full_join(areas %>% st_simplify(preserveTopology=T, dTolerance=2000), 
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

################

# Temporal sensitivity to k

ks <- c(0.001, 0.01, 0.1, 1)

k_fits <- lapply(ks, function(k) inla(stops ~ offset(log(arrests + k)) +
                                        f(ethnicity, constr=TRUE) + 
                                        f(force, constr=TRUE) + 
                                        f(year, constr=TRUE) +
                                        f(crime, constr=TRUE) + 
                                        f(interaction(ethnicity,year), constr=TRUE) +
                                        f(interaction(ethnicity, force), constr=TRUE) +
                                        f(interaction(force, year), constr=TRUE) +
                                        f(obs, constr=TRUE),
                                      family = "poisson",
                                      data = data,
                                      lincomb = lin.combs.eth.year,
                                      control.compute = list(config = TRUE),
                                      control.predictor = list(compute = TRUE)
))

k.lincomb.result <- lapply(k_fits, function(k_fit) lapply(k_fit$marginals.lincomb.derived,
                                                          function(m) as.data.frame(inla.zmarginal(inla.tmarginal(exp, m)))))


k.lincomb.result[[1]]  

k.lincomb.df <- do.call(rbind, lapply(1:4,
                                      function(i) do.call(rbind, k.lincomb.result[[i]]) %>%
                                        rownames_to_column("ID") %>%
                                        mutate(
                                          ID = as.integer(substr(ID, 3,4)),
                                          ethnicity = c("Asian", "Black")[as.integer(ID > 8)+1],
                                          year = ((ID-1) %% 8) + 2016,
                                          k = ks[i]
                                        ) ))

k.lincomb.df  %>%
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

ggsave("visualisations/k_sensitivity_temporal.pdf", width=6, height=3, units="in")

###########

# K excursions

k.samples <- lapply(k_fits,
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

k.excursion.asian <- lapply(k.ready.for.excursions.asian, function(df)
  rownames(df)[as.logical((df %>%
    excursions.mc(
      alpha = 0.05,
      u = 0,
      type = ">"
    ))$E)])
  
k.excursion.asian


##########


# df1: crime interactions
# df2: 0.001
# df3: diffuse priors

df5 <- do.call(rbind, lincomb.result) %>%
  rownames_to_column("ID") %>%
  mutate(
    ID = as.integer(substr(ID, 3,4)),
    ethnicity = c("Asian", "Black")[as.integer(ID > 8)+1],
    year = ((ID-1) %% 8) + 2016
  ) 

df5 %>%
  ggplot(aes(x = year,color = ethnicity, linetype = ethnicity)) + 
  geom_line(aes(y = mean))  +
  geom_errorbar(aes(ymin = quant0.025, ymax=quant0.975),
                position = position_dodge(width = 0.6)) +
  geom_hline(aes(yintercept = 1, color="White", linetype = "White")) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = rel(1), color = "black")
  ) +
  scale_color_manual(values = c("black", "black", "red")) +
  scale_linetype_manual(values = c("solid", "dotted", "solid")) +
  labs(
    x = "Financial year beginning",
    y = "Posterior Ratio",
    colour = "Ethnicity",
    linetype = "Ethnicity"
  )

ggsave("visualisations/posterior_ratio_k_0_01.pdf", width=6, height=3, units="in")

df1$config <- "Crime\ninteractions"
df2$config <- "Arrests\n+0.001"
df3$config <- "Diffuse\npriors"
df4$config <- "All\nchanges"

rbind(df1, df2, df3, df4) %>%
  rename(Ethnicity = ethnicity) %>%
  mutate(
    config = factor(config, levels = c("Diffuse\npriors", "Crime\ninteractions", "Arrests\n+0.001", "All\nchanges"))
  ) %>%
  ggplot(aes(x = year,color = Ethnicity, linetype = Ethnicity)) + 
  geom_line(aes(y = mean))  +
  geom_errorbar(aes(ymin = quant0.025, ymax=quant0.975),
                position = position_dodge(width = 0.4)) +
  geom_hline(aes(yintercept = 1, color="White", linetype = "White")) +
  facet_wrap(vars(config), nrow=1) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = rel(1), color = "black")
  ) +
  scale_color_manual(values = c("black", "black", "red")) +
  scale_linetype_manual(values = c("solid", "dotted", "solid")) +
  scale_x_continuous(breaks = c(2017, 2020)) +
  labs(
    x = "Financial year beginning",
    y = "Ratio"
  )

ggsave("visualisations/temporal_sensitivity.pdf", width=6, height=4, units="in")



#########


data <- read_csv("data/cleaned/stopsearch_arrests.csv") %>%
  rownames_to_column("obs") %>%
  mutate(
    force = factor(police_force),
    ethnicity = factor(officer_defined_ethnicity),
    crime = factor(crime_type),
    year = factor(previous_financial_year),
    stops = number_of_stops,
    arrests = number_of_arrests,
    obs = factor(obs),
    asian_perc = scale(asian_perc),
    black_perc = scale(black_perc)
  )

fit <- inla(stops ~ offset(log(arrests + 1)) +
              asian_perc +
              black_perc +
              f(ethnicity) + 
              f(force) + 
              f(year) +
              f(crime) + 
              f(interaction(ethnicity,year)) +
              f(interaction(ethnicity, force)) +
              f(interaction(force, year)) +
              f(interaction(ethnicity, crime)) +
              f(interaction(crime, year)) +
              f(interaction(crime, force)) +
              f(obs),
            family = "poisson",
            data = data,
            lincomb = lin.combs.eth.year,
            control.compute = list(config = TRUE),
            control.predictor = list(compute = TRUE)
)



#################


###################

# Analysis of epsilon

plot(fit$summary.random$obs$mean, xlim=c(1100, 1200))

data[1100:1200,]$force

epsilons <- fit$summary.random$obs %>%
  mutate(ID = as.integer(ID)) %>%
  arrange(ID) %>%
  select(mean)

epsilon.df <- data.frame(
  epsilon = epsilons[which(data$force == "lancashire"), ],
  ethnicity = data$ethnicity[which(data$force == "lancashire")],
  year = data$year[which(data$force == "lancashire")],
  crime = data$crime[which(data$force == "lancashire")]
) %>%
  mutate(
    year = as.integer(substr(year, 1, 4)) + 1
  )

ggplot(epsilon.df, aes(x = year, y = epsilon, colour = str_to_title(crime), linetype = str_to_title(ethnicity))) +
  geom_line() +
  theme_classic() +
  labs(
    x = "Financial year beginning",
    y = "Posterior mean of epsilon",
    colour = "Crime type",
    linetype = "Ethnicity"
  )

ggsave("visualisations/fitted_epsilon.pdf", width=6, height=3, units="in")


