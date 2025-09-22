data$year2 <- data$year
data$year3 <- data$year
data$year4 <- data$year

pc.prior <- list(prec = list(prior = "pc.prec", param = c(1, 0.5)))

model.2 <- inla(stops ~
       f(ethnicity, constr=T, hyper = pc.prior) + f(force, constr=T, hyper = pc.prior) + f(crime, constr=T, hyper = pc.prior) +
      f(year4, model = "ar1", constr=T, hyper = pc.prior) +
       f(year, model = "ar1", group = as.numeric(ethnicity), constr=T, hyper = pc.prior) +
       f(year2, model = "ar1", group = as.numeric(force), constr=T, hyper = pc.prior) +
       f(year3, model = "ar1", group = as.numeric(crime), constr=T, hyper = pc.prior) +
       f(interaction(ethnicity, force), constr=T, hyper = pc.prior) +
       f(interaction(force, crime), constr=T, hyper = pc.prior) + 
       f(interaction(ethnicity, crime), constr=T, hyper = pc.prior) +
        f(obs, constr=T, hyper = pc.prior),
     data = data,
     family = "poisson",
     E = arrests+0.01,
     control.compute = list(config=T),
     control.predictor = list(compute = T))

model <- model.2

exsets <- compute.overall.excursion.sets(model, data)

plot.overall.excursion.sets(exsets)