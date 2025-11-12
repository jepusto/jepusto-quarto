library(tidyverse)
library(puniform)
library(metafor)

k <- 300
mu <- 0.2
tau <- 0.1
lambda <- 0.1
curve(dbeta(2 * x, shape1 = 3, shape2 = 1), from = 0, to = 0.5)
sigma_i <- rbeta(k, shape1 = 3, shape2 = 1) / 2
delta_i <- rnorm(k, mean = mu, sd = tau)
Ti <- rnorm(k, mean = delta_i, sd = sigma_i)
Si <- Ti / sigma_i > qnorm(0.975)
sel_prob <- (1 - Si) * lambda + Si
obs <- as.logical(rbinom(k, size = 1, prob = sel_prob))

Ti <- Ti[obs]
sigma_i <- sigma_i[obs]
Si <- Si[obs]

dat <- data.frame(Ti, sigma_i, Si)
ggplot(dat) + 
  aes(Ti, sigma_i, color = Si) + 
  geom_point() + 
  scale_y_reverse(limits = c(0.5, 0))

pUs <- puni_star(yi = Ti, vi = sigma_i^2, side = "right")

PSM <- 
  rma.uni(yi = Ti, sei = sigma_i, method = "ML") |>
  selmodel(type = "step", steps = .025)

pUs$est
PSM$beta

pUs$tau2
PSM$tau2

pUs
PSM

ll_profile <- function(mu, tau, sigma_i, Ti, Si) {
  eta_i <- sqrt(tau^2 + sigma_i^2)
  Bi <- pnorm((sigma_i * qnorm(0.975) - mu) / eta_i)
  k1 <- sum(1 - Si)
  lambda <- uniroot(\(lambda) k1 - sum(lambda * Bi / (1 - (1 - lambda) * Bi)), interval = c(1e-5, 1e2))$root
  Ai <- 1 - Bi + lambda * Bi
  
  - sum((Ti - mu)^2 / (2 * eta_i^2)) - sum(log(eta_i)) + sum(1 - Si) * log(lambda) - sum(log(Ai))
}

ll_pUs <- function(mu, tau, sigma_i, Ti, Si) {
  eta_i <- sqrt(tau^2 + sigma_i^2)
  Bi <- pnorm((sigma_i * qnorm(0.975) - mu) / eta_i)
  
  - sum((Ti - mu)^2 / (2 * eta_i^2)) - sum(log(eta_i)) - sum((1 - Si) * log(Bi)) - sum(Si * log(1 - Bi))
}

Diff <- function(mu, tau, sigma_i, Si) {
  eta_i <- sqrt(tau^2 + sigma_i^2)
  Bi <- pnorm((sigma_i * qnorm(0.975) - mu) / eta_i)
  k1 <- sum(1 - Si)
  lambda <- uniroot(\(lambda) k1 - sum(lambda * Bi / (1 - (1 - lambda) * Bi)), interval = c(1e-5, 1e2))$root
  Ai <- 1 - Bi + lambda * Bi
  
  sum(1 - Si) * log(lambda) - sum(log(Ai)) + sum((1 - Si) * log(Bi)) + sum(Si * log(1 - Bi))
}

param_grid <- expand_grid(mu = seq(0,0.3,length.out = 100), tau = seq(0,0.2, length.out = 80))

ll_eval <- 
  param_grid %>%
  mutate(
    ll_prof = pmap_dbl(list(mu,tau), ll_profile, sigma_i = sigma_i, Ti = Ti, Si = Si),
    ll_pUs = pmap_dbl(list(mu,tau), ll_pUs, sigma_i = sigma_i, Ti = Ti, Si = Si),
    Diff = pmap_dbl(list(mu,tau), Diff, sigma_i = sigma_i, Si = Si),
    check = ll_prof - ll_pUs - Diff
  )

summary(ll_eval$check)

ggplot(ll_eval) + 
  aes(mu, tau, z = Diff) + 
  geom_contour_filled()

ggplot(ll_eval) + 
  aes(mu, tau, z = Diff) + 
  geom_raster(aes(fill = Diff)) +
  geom_contour(colour = "white", binwidth = 0.1)

confint(PSM, fixed = TRUE)
profile(PSM, delta = 2)
