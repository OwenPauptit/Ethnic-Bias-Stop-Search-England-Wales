data$year2 <- data$year
data$year3 <- data$year
data$year4 <- data$year

pc.prec <- function(sigma, alpha) {
  return(list(prior = "pc.prec", param = c(sigma, alpha)))
}

iid.prior <- function(sigma, alpha) {
  return(list(prec = pc.prec(sigma, alpha)))
}

ar.prior <- function(sigma, sigma.alpha, rho, rho.alpha, base.rho.zero = T) {
  if (base.rho.zero) {
    return(list(prec = pc.prec(sigma, alpha),
                rho = list(prior = "pc.cor0", param = c(rho, rho.alpha))))
  }
  return(list(prec = pc.prec(sigma, alpha),
              rho = list(prior = "pc.cor1", param = c(rho, rho.alpha))))
}


prec.prior <- list(prec = list(prior = "pc.prec", param = c(1, 0.5)))

ar.prior <- list(prec = list(prior = "pc.prec", param = c(1, 0.5)),
                 rho = list(prior = "pc.cor0", param = c(0.5, 0.1)))

ethnicity.indices <- c(asian = 1, black = 2, white = 3)
data$ethnicity.index <- ethnicity.indices[data$ethnicity]
data$force.index <- as.numeric(data$force)
data$crime.index <- as.numeric(data$crime)

lincomb_list <- list()
num.yrs <- length(unique(data$year))
num.eth.yr.combos <- length(ethnicity.indices) * num.yrs

for (e in unique(data$ethnicity)) {
  for (t in unique(data$year)) {
    # Create a name for the linear combination
    lincomb_name <- paste("ethnicity", e, "year", t, sep="_")
    
    eth <- c(0,0,0)
    eth[ethnicity.indices[e]] <- 1
    
    yr <- rep(0, num.eth.yr.combos)
    yr[(ethnicity.indices[e]-1)*num.yrs + t-2014] <- 1
    # Define the linear combination
    lincomb_list[[lincomb_name]] <- inla.make.lincomb(
      c(ethnicity = eth, year2 = yr)
    )
  }
}

lincomb_list


model <- inla(stops ~
       f(ethnicity, constr=T, hyper = prec.prior) + f(force, constr=T, hyper = prec.prior) + f(crime, constr=T, hyper = prec.prior) +
      f(year, model = "ar1", constr=T, hyper = ar.prior) +
       f(year2, model = "ar1", group = ethnicity.index, constr=T, hyper = ar.prior) +
       f(year3, model = "ar1", group = force.index, constr=T, hyper = ar.prior) +
       f(year4, model = "ar1", group = crime.index, constr=T, hyper = ar.prior) +
       f(interaction(ethnicity, force), constr=T, hyper = prec.prior) +
       f(interaction(force, crime), constr=T, hyper = prec.prior) + 
       f(interaction(ethnicity, crime), constr=T, hyper = prec.prior) +
        f(obs, constr=T, hyper = prec.prior),
     data = data,
     family = "poisson",
     E = arrests+0.01,
     lincomb = lin.combs.eth.year,
     control.compute = list(config=T),
     control.predictor = list(compute = T))



exsets <- compute.overall.excursion.sets(model, data)

plot.overall.excursion.sets(exsets, areas)

exsets


library(sf)
