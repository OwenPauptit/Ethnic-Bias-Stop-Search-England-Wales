data$year2 <- data$year
data$year3 <- data$year
data$year4 <- data$year

prec.prior <- list(prec = list(prior = "pc.prec", param = c(1, 0.5)))

ar.prior <- list(prec = list(prior = "pc.prec", param = c(1, 0.5)),
                 rho = list(prior = "pc.cor0", param = c(0.5, 0.1)))

model <- inla(stops ~
       f(ethnicity, constr=T, hyper = prec.prior) + f(force, constr=T, hyper = prec.prior) + f(crime, constr=T, hyper = prec.prior) +
      f(year4, model = "ar1", constr=T, hyper = ar.prior) +
       f(year, model = "ar1", group = as.numeric(ethnicity), constr=T, hyper = ar.prior) +
       f(year2, model = "ar1", group = as.numeric(force), constr=T, hyper = ar.prior) +
       f(year3, model = "ar1", group = as.numeric(crime), constr=T, hyper = ar.prior) +
       f(interaction(ethnicity, force), constr=T, hyper = prec.prior) +
       f(interaction(force, crime), constr=T, hyper = prec.prior) + 
       f(interaction(ethnicity, crime), constr=T, hyper = prec.prior) +
        f(obs, constr=T, hyper = prec.prior),
     data = data,
     family = "poisson",
     E = arrests+0.01,
     control.compute = list(config=T),
     control.predictor = list(compute = T))

exsets <- compute.overall.excursion.sets(model, data)

plot.overall.excursion.sets(exsets, areas)

exsets


library(sf)
