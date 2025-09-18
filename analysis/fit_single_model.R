library(INLA)
library(tidyr)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)
library(excursions)
library(sf)
library(patchwork)

load.stop.search.data <- function() {
  return(read_csv("../data/stopsearch_arrests.csv") %>%
  rownames_to_column("obs") %>%
  mutate(
    force = factor(police_force),
    ethnicity = factor(officer_defined_ethnicity),
    crime = factor(crime_type),
    year = as.integer(substr(previous_financial_year, 1,4)),
    stops = number_of_stops,
    arrests = number_of_arrests,
    obs = factor(obs)
  ) %>%
  select(ethnicity, force, crime, year, stops, arrests, obs))
}

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

# fit.single.model <- function(data) {
# 
#   lincomb <- get.eth.year.lincomb(data)
#   
#   model <- inla(stops ~ offset(log(arrests + 0.01)) +
#                   f(ethnicity, constr=TRUE) + 
#                   f(force, constr=TRUE) + 
#                   f(year, model = "ar1", constr=TRUE) +
#                   f(crime, constr=TRUE) + 
#                   f(interaction(ethnicity,year), constr=TRUE) +
#                   f(interaction(ethnicity, force), constr=TRUE) +
#                   f(interaction(force, year), constr=TRUE) +
#                   f(interaction(crime, ethnicity), constr=TRUE) + 
#                   f(interaction(crime, year), constr=TRUE) +
#                   f(interaction(crime, force), constr=TRUE) +
#                   f(obs, constr=TRUE),
#                 family = "poisson",
#                 data = data,
#                 lincomb = lincomb,
#                 control.compute = list(config = TRUE),
#                 control.predictor = list(compute = TRUE))
#   
#   return(model)
# }

fit.single.model<- function(data, k = 0.01, time_model = "iid", iid_prior = list(prior = "loggamma", param = c(1, 0.0005))) {
  
  lincomb <- get.eth.year.lincomb(data)
  
  model <- inla(stops ~ offset(log(arrests + k)) +
                  f(ethnicity, constr=TRUE, hyper = list(prec = iid_prior)) + 
                  f(force, constr=TRUE, hyper = list(prec = iid_prior)) + 
                  f(year, model = time_model, constr=TRUE, hyper = list(prec = iid_prior)) +
                  f(crime, constr=TRUE, hyper = list(prec = iid_prior)) +
                  f(interaction(ethnicity,year), constr=TRUE, hyper = list(prec = iid_prior)) +
                  f(interaction(ethnicity, force), constr=TRUE, hyper = list(prec = iid_prior)) +
                  f(interaction(force, year), constr=TRUE, hyper = list(prec = iid_prior)) +
                  f(interaction(crime, ethnicity), constr=TRUE, hyper = list(prec = iid_prior)) +
                  f(interaction(crime, year), constr=TRUE, hyper = list(prec = iid_prior)) +
                  f(interaction(crime, force), constr=TRUE, hyper = list(prec = iid_prior)) +
                  f(obs, constr=TRUE),
                family = "poisson",
                data = data,
                lincomb = lincomb,
                control.compute = list(config = TRUE),
                control.predictor = list(compute = TRUE))
  
  return(model)
}

