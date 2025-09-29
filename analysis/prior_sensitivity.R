# source("analysis/fit_single_model.R")
# source("analysis/initial_analysis.R")

pc.prec <- function(sigma, alpha) {
  return(list(prior = "pc.prec", param = c(sigma, alpha)))
}

iid.prior <- function(sigma, alpha) {
  return(list(prec = pc.prec(sigma, alpha)))
}

ar.prior <- function(sigma, sigma.alpha, rho, rho.alpha, base.rho.zero = T) {
  if (base.rho.zero) {
    return(list(prec = pc.prec(sigma, sigma.alpha),
                rho = list(prior = "pc.cor0", param = c(rho, rho.alpha))))
  }
  return(list(prec = pc.prec(sigma, sigma.alpha),
              rho = list(prior = "pc.cor1", param = c(rho, rho.alpha))))
}

# Default priors.
# Cor1 for AR
# PC with small and large

fit.models.sensitivity.pc.cor0 <- function(data) {
  
  alpha <- 0.9
  sigmas <- c(0.1, 10, 100)
  rhos <- c(0.1, 0.5, 0.9)

  priors.list <- lapply(1:3,
                        function(idx) list(
                          iid = iid.prior(sigmas[idx], alpha),
                          ar = ar.prior(sigmas[idx], alpha, rhos[idx], alpha)
                        ))

  models <- lapply(priors.list,
                   function(priors)
                     fit.single.model(data, prec.prior = priors$iid, ar.prior = priors$ar))

  return(models)
}





