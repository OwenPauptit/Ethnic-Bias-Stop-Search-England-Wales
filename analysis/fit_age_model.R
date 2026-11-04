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

load.stop.search.data.age <- function() {
  return(read_csv("../data/stopsearch_arrests_with_age.csv") %>%
           rownames_to_column("obs") %>%
           mutate(
             force = factor(police_force),
             ethnicity = factor(officer_defined_ethnicity),
             crime = factor(crime_type),
             year = as.integer(substr(previous_financial_year, 1,4)),
             stops = number_of_stops,
             arrests = number_of_arrests,
             age = factor(age),
             obs = factor(obs)
           ) %>%
           select(ethnicity, force, crime, year, stops, arrests, age, obs) %>%
           mutate(
             force.idx = as.numeric(force),
             ethnicity.idx = ethnicity.indices[ethnicity],
             crime.idx = as.numeric(crime),
             age.idx = as.numeric(age),
             year.ethnicity = year,
             year.crime = year,
             year.force = year,
             year.age = year
           ))
}

fit.age.model<- function(data, 
                            k = 0.01, 
                            prec.prior = list(prec = list(prior = "pc.prec", param = c(1, 0.5))),
                            ar.prior = list(prec = list(prior = "pc.prec", param = c(1, 0.5)),
                                            rho = list(prior = "pc.cor0", param = c(0.5, 0.1)))) {
  
  lincomb <- get.eth.year.lincomb(data)
  
  model <- inla(stops ~ offset(log(arrests + k)) + # ugly to have it here but makes posterior predictive checks easier
                  f(ethnicity, constr=T, hyper = prec.prior) + 
                  f(force, constr=T, hyper = prec.prior) + 
                  f(crime, constr=T, hyper = prec.prior) +
                  f(age, constr=T, hyper = prec.prior) +
                  f(year, model = "ar1", constr=T, hyper = ar.prior) +
                  f(year.ethnicity, model = "ar1", group = ethnicity.idx, constr=T, hyper = ar.prior) +
                  f(year.force, model = "ar1", group = force.idx, constr=T, hyper = ar.prior) +
                  f(year.crime, model = "ar1", group = crime.idx, constr=T, hyper = ar.prior) +
                  f(year.age, model = "ar1", group = age.idx, constr=T, hyper = ar.prior) +
                  f(interaction(ethnicity, force), constr=T, hyper = prec.prior) +
                  f(interaction(force, crime), constr=T, hyper = prec.prior) +
                  f(interaction(ethnicity, crime), constr=T, hyper = prec.prior) +
                  f(interaction(ethnicity, age), constr=T, hyper=prec.prior) +
                  f(interaction(force, age), constr=T, hyper=prec.prior) +
                  f(interaction(crime, age), constr=T, hyper=prec.prior) +
                  f(obs, constr=T, hyper = prec.prior),
                data = data,
                family = "poisson",
                # E = arrests+k,   # prettier place to have offset, but makes posterior predictive checks more complicated
                lincomb = lincomb,
                control.compute = list(config=T),
                control.predictor = list(compute = T))
  
  return(model)
}

