library(conflicted)
library(tidyverse)
conflicts_prefer(dplyr::filter, .quiet = TRUE)

library(metadat)
library(metafor)
library(statmod)

data("dat.crede2010", package = "metadat")
dat <- 
  dat.crede2010 %>%
  mutate(
    ri = if_else(ri < -0.1, -ri, ri) 
  ) %>%
  escalc(measure="ZCOR", ri=ri, ni=ni, data = ., var.names = c("z", "Vz")) %>%
  escalc(measure="COR", ri=ri, ni=ni, data = ., var.names = c("r", "Vr"))

k <- nrow(dat)

#-------------------------------------------------------------------------------
# Pearson's r

res_r <- rma(yi = r, vi = Vr, data = dat)
summary(res_r)
funnel(res_r)


d_r <- function(ri, Ni, mu, tau, log = FALSE, points = 99) {
  if (is.infinite(Ni)) {
    dens_ri <- dnorm(ri, mean = mu, sd = tau)
    Const <- diff(pnorm(c(-1,1), mean = mu, sd = tau))
  } else {
    qp <- gauss.quad.prob(n = points, dist = "normal", mu = mu, sigma = tau)
    sub <- which(-1 < qp$nodes & qp$nodes < 1)
    wt <- qp$weights[sub]
    rho <- qp$nodes[sub]
    dens_ri <- sapply(ri, \(r) sum(wt * dnorm(r, mean = rho, sd = (1 - rho^2) / sqrt(Ni))))
    Const <- sum(wt)
  }
  if (log) {
    log(dens_ri) - log(Const)
  } else {
    dens_ri / Const
  }
}


f_lpd_r <- function(i, mod, points = 99) {
  id <- 1:mod$k
  mod_i <- update(mod, subset = id != i)
  lpd <- d_r(
    ri = mod$yi[i], 
    Ni = mod$data$ni[i], 
    mu = as.numeric(mod_i$b), 
    tau = sqrt(mod_i$tau2), 
    log = TRUE,
    points = points
  )
  data.frame(
    mu_r = as.numeric(mod_i$b),
    tau_r = sqrt(mod_i$tau2),
    lpd_r = lpd
  )
}

dat <- bind_cols(dat, map_dfr(1:k, f_lpd_r, mod = res_r, points = 499))
summary(dat$lpd_r)
mean(dat$lpd_r)

#-------------------------------------------------------------------------------
# Fisher's z

res_Z <- rma(yi = z, vi = Vz, data = dat)
summary(res_Z)
funnel(res_Z)

d_Z <- function(ri, Ni, mu, tau, log = FALSE) {
  zi <- atanh(ri)
  sd <- sqrt(tau^2 + 1 / (Ni - 3))
  jac_const <- 1 / (1 - ri^2)
  if (log) {
    dnorm(zi, mean = mu, sd = sd, log = TRUE) + log(jac_const)
  } else {
    dnorm(zi, mean = mu, sd = sd) * jac_const
  }
}

f_lpd_Z <- function(i, mod) {
  id <- 1:mod$k
  mod_i <- update(mod, subset = id != i)
  lpd <- d_Z(
    ri = tanh(mod$yi[i]), 
    Ni = mod$data$ni[i], 
    mu = as.numeric(mod_i$b), 
    tau = sqrt(mod_i$tau2), 
    log = TRUE
  )
  data.frame(
    mu_z = as.numeric(mod_i$b),
    tau_z = sqrt(mod_i$tau2),
    lpd_z = lpd
  )
}

dat <- bind_cols(dat, map_dfr(1:k, f_lpd_Z, mod = res_Z))
summary(dat$lpd_z)
mean(dat$lpd_z)

dat$diff_lpd <- with(dat, lpd_z - lpd_r)
summary(dat$diff_lpd)
mean(dat$diff_lpd)
sd(dat$diff_lpd) / sqrt(k)

lpd_dat <- 
  dat %>%
  select(studyid, ni, ri, matches("_(r|z)$")) %>%
  pivot_longer(
    matches("_(r|z)$"), 
    names_to = c(".value","metric"), names_pattern = "(.+)_(.)"
  )

ggplot(lpd_dat) + 
  aes(lpd, color = metric, fill = metric) + 
  geom_density(alpha = 0.5) +
  geom_rug() + 
  labs(
    x = "log predictive density contribution",
    y = ""
  ) + 
  theme_minimal()

ggplot(dat) + 
  aes(ri, 1 / sqrt(ni), color = diff_lpd) + 
  geom_hline(yintercept = 0) + 
  geom_point() +
  scale_y_reverse(expand = expansion(0, c(0.02,0))) + 
  labs(
    x = expression(r[i]),
    y = expression(1 / sqrt(N[i]))
  ) + 
  theme_minimal()

outliers <- 
  dat %>%
  filter(diff_lpd < -1) %>%
  select(studyid, ni, ri, mu_r, tau_r, mu_z, tau_z) %>%
  cross_join(tibble(r = seq(-0.99,0.99,0.005))) %>%
  rowwise() %>%
  mutate(
    studyid = paste("Study", studyid),
    dens_r = d_r(ri = r, Ni = ni, mu = mu_r, tau = tau_r, points = 499),
    dens_z = d_Z(ri = r, Ni = ni, mu = mu_z, tau = tau_z)
  ) %>%
  ungroup() %>%
  pivot_longer(
    matches("_(r|z)$"), 
    names_to = c(".value","metric"), names_pattern = "(.+)_(.)"
  )

ggplot(outliers) + 
  aes(x = r, y = dens, color = metric, fill = metric) + 
  facet_wrap(~ studyid) + 
  geom_area(alpha = 0.5, position = position_identity()) +
  geom_vline(aes(xintercept = ri)) + 
  scale_x_continuous(expand = expansion(0,0)) + 
  scale_y_continuous(expand = expansion(0,c(0,0.05))) + 
  theme_minimal()

