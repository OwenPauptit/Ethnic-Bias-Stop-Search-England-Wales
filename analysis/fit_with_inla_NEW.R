library(INLA)
library(tidyr)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)
library(excursions)
library(sf)
library(patchwork)


set.seed(4129264)

data <- read_csv("data/stopsearch_arrests.csv") %>%
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


start_time <- Sys.time()

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

get.eth.year.lincomb <- function(data) {
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
  
  return( lin.combs.eth.year )
}

lincomb <- get.eth.year.lincomb(data)

model <- inla(stops ~ offset(log(arrests + 0.01)) +
                f(ethnicity, constr=TRUE) + 
                f(force, constr=TRUE) + 
                f(year, model = "rw1", constr=TRUE) +
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
              lincomb = lincomb,
              control.compute = list(config = TRUE),
              control.predictor = list(compute = TRUE))

summary(model)

# Temporal plot

lincomb.result <- lapply(model$marginals.lincomb.derived,
                         function(m) as.data.frame(inla.zmarginal(inla.tmarginal(exp, m))))

do.call(rbind, lincomb.result) %>%
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

# Overall Excursion sets 

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

run_time <- difftime(Sys.time(), start_time)

run_time

# Load area info

load_police_force_kml <- function(filename) {
  path <- paste0("data/force kmls/", filename)
  area <- st_read(path)
  police_force <- gsub("-", "_", filename)
  police_force <- substr(police_force, 1, nchar(filename)-4)
  area$police_force <- police_force
  return(area)
}

file_names <- list.files(path = "data/force kmls")

areas <- do.call(rbind, lapply(file_names, load_police_force_kml)) %>%
  st_simplify(dTolerance = 1000)

# Plot maps

p1 <- areas %>%
  mutate(
    excursion = police_force %in% asian.forces.exset
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
  scale_fill_manual(values = c("white", "red")) +
  ggtitle("Asian")

p2 <- areas %>%
  mutate(
    excursion = police_force %in% black.forces.exset
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
  scale_fill_manual(values = c("white", "red")) +
  ggtitle("Black")

p1 + p2
