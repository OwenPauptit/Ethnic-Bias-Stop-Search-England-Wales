library(INLA)
library(tidyr)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)
library(excursions)
library(sf)
library(patchwork)
library(stringr)

ethnicity.indices <- c(asian = 1, black = 2, white = 3)

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
           select(ethnicity, force, crime, year, stops, arrests, obs) %>%
           mutate(
             force.idx = as.numeric(force),
             ethnicity.idx = ethnicity.indices[ethnicity],
             crime.idx = as.numeric(crime),
             yyear = year,
             yyyear = year,
             yyyyear = year
           ))
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
    yyear = weights.eth.year
  )
  
  return( lin.combs.eth.year )
}

fit.single.model<- function(data, 
                            k = 0.01, 
                            prec.prior = list(prec = list(prior = "pc.prec", param = c(1, 0.5))),
                            ar.prior = list(prec = list(prior = "pc.prec", param = c(1, 0.5)),
                                            rho = list(prior = "pc.cor0", param = c(0.5, 0.1)))) {
  
  lincomb <- get.eth.year.lincomb(data)
  
  model <- inla(stops ~ offset(log(arrests + k)) + # ugly to have it here but makes posterior predictive checks easier
                  f(ethnicity, constr=T, hyper = prec.prior) + 
                  f(force, constr=T, hyper = prec.prior) + 
                  f(crime, constr=T, hyper = prec.prior) +
                  f(year, model = "ar1", constr=T, hyper = ar.prior) +
                  f(yyear, model = "ar1", group = ethnicity.idx, constr=T, hyper = ar.prior) +
                  f(yyyear, model = "ar1", group = force.idx, constr=T, hyper = ar.prior) +
                  f(yyyyear, model = "ar1", group = crime.idx, constr=T, hyper = ar.prior) +
                  f(interaction(ethnicity, force), constr=T, hyper = prec.prior) +
                  f(interaction(force, crime), constr=T, hyper = prec.prior) + 
                  f(interaction(ethnicity, crime), constr=T, hyper = prec.prior) +
                  f(obs, constr=T, hyper = prec.prior),
                data = data,
                family = "poisson",
                # E = arrests+k,   # prettier place to have offset, but makes posterior predictive checks more complicated
                lincomb = lincomb,
                control.compute = list(config=T),
                control.predictor = list(compute = T))
  
  return(model)
}

