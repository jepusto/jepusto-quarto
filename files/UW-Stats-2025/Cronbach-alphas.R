library(conflicted)
library(tidyverse)
conflicts_prefer(dplyr::filter, .quiet = TRUE)

library(metadat)
library(metafor)
library(statmod)

data("dat.demir2022", package = "metadat")
dat <- 
  dat.demir2022 %>%
  escalc(measure="ARAW", ai=alpha, ni=n, mi=items, data = ., var.names = c("a", "Va")) %>%
  escalc(measure="ABT", ai=alpha, ni=n, mi=items, data = ., var.names = c("b", "Vb")) %>%
  escalc(measure="AHW", ai=alpha, ni=n, mi=items, data = ., var.names = c("hw", "Vhw")) %>%
  mutate(
    Vb_check = 2 * items / ((items - 1) * (n - 2)),
    Va_check = Vb_check * (1 - alpha)^2,
    Vhw_check = 18 * (n - 1) * items * (1 - alpha)^(2/3) / ((items - 1) * (9 * n - 11)^2),
  )

k <- nrow(dat)

loo_fit <- function(i, mod) {
  id <- 1:mod$k
  mod_i <- update(mod, subset = id != i)
  data.frame(
    mu = as.numeric(mod_i$b),
    tau = sqrt(mod_i$tau2)
  )
}

CI_PI <- function(mod,..., CI_level = 95, PI_level = 80) {
  CI_res <- 
    predict(mod, ..., level = CI_level) %>%
    as.data.frame() %>%
    remove_rownames()
  PI_res <- predict(mod, ..., level = PI_level) %>%
    as.data.frame() %>%
    remove_rownames()
  
  CI_res %>%
    select(Est = pred, ci.lb, ci.ub) %>%
    bind_cols(select(PI_res, pi.lb, pi.ub))
}

#-------------------------------------------------------------------------------
# Raw alpha

res_a <- rma(yi = a, vi = Va, data = dat)
summary(res_a)
funnel(res_a)
a_est <- CI_PI(res_a)

d_a <- function(ai, Ni, qi, mu, tau, log = FALSE, points = 99) {
  if (is.infinite(Ni)) {
    dens_ai <- dnorm(ai, mean = mu, sd = tau)
    Const <- diff(pnorm(c(-1,1), mean = mu, sd = tau))
  } else {
    sd_b <- sqrt(2 * qi / ((qi - 1) * (Ni - 2)))
    # qp <- gauss.quad.prob(n = points, dist = "normal", mu = mu, sigma = tau)
    # sub <- which(0 < qp$nodes & qp$nodes < 1)
    # wt <- qp$weights[sub]
    # alpha <- qp$nodes[sub]
    # dens_ai <- sapply(ai, \(a) sum(wt * dnorm(a, mean = alpha, sd = (1 - alpha) * sd_b )))
    # Const <- sum(wt)
    dens_ai <- sapply(
      ai, 
      \(a) integrate(
        \(alpha) dnorm(a, mean = alpha, sd = (1 - alpha) * sd_b) * dnorm(alpha, mean = mu, sd = tau), 
        lower = 0, upper = 1 - 1e-6
      )$value
    )
    Const <- diff(pnorm(c(0,1), mean = mu, sd = tau))
  }
  if (log) {
    log(dens_ai) - log(Const)
  } else {
    dens_ai / Const
  }
}

dat_a <- 
  dat %>%
  mutate(
    mod_a = map_dfr(1:k, loo_fit, mod = res_a)
  ) %>%
  unnest(mod_a, names_sep = "_") %>%
  rowwise() %>%
  mutate(
    lpd_a = d_a(
      ai = a, Ni = n, qi = items, 
      mu = mod_a_mu, tau = mod_a_tau, 
      log = TRUE
    )
  ) %>%
  ungroup()

#-------------------------------------------------------------------------------
# Bonett (2002) transformation of alpha

res_b <- rma(yi = b, vi = Vb, data = dat)
summary(res_b)
funnel(res_b)
b_est <- CI_PI(res_b, transf = transf.iabt)

d_b <- function(ai, Ni, qi, mu, tau, log = FALSE, points = 99) {
    bi <- -log(1 - ai)
    sd <- if (is.infinite(Ni)) tau else sqrt(tau^2 + 2 * qi / ((qi - 1) * (Ni - 2)))
    jac_const <- 1 / (1 - ai)
    if (log) {
      dnorm(bi, mean = mu, sd = sd, log = TRUE) + log(jac_const)
    } else {
      dnorm(bi, mean = mu, sd = sd) * jac_const
    }
}

dat_b <- 
  dat_a %>%
  mutate(
    mod_b = map_dfr(1:k, loo_fit, mod = res_b)
  ) %>%
  unnest(mod_b, names_sep = "_") %>%
  rowwise() %>%
  mutate(
    lpd_b = d_b(
      ai = a, Ni = n, qi = items, 
      mu = mod_b_mu, tau = mod_b_tau, 
      log = TRUE
    )
  ) %>%
  ungroup()

#-------------------------------------------------------------------------------
# Hakstian & Whalen (1976) transformation of alpha

res_hw <- rma(yi = hw, vi = Vhw, data = dat)
summary(res_hw)
funnel(res_hw)
hw_est <- CI_PI(res_hw, transf = transf.iahw)

d_hw <- function(ai, Ni, qi, mu, tau, log = FALSE, points = 99) {

  hw <- 1 - (1 - ai)^(1/3)  
  jac_const <- (1 - ai)^(-2/3) / 3
    
  if (is.infinite(Ni)) {
    dens_ai <- dnorm(hw, mean = mu, sd = tau)
    Const <- diff(pnorm(c(0,1), mean = mu, sd = tau))
  } else {
    sd_b <- sqrt(18 * (Ni - 1) * qi / ((qi - 1) * (9 * Ni - 11)^2))
    # qp <- gauss.quad.prob(n = points, dist = "normal", mu = mu, sigma = tau)
    # sub <- which(0 < qp$nodes & qp$nodes < 1)
    # wt <- qp$weights[sub]
    # alpha <- qp$nodes[sub]
    # dens_ai <- sapply(hw, \(a) sum(wt * dnorm(a, mean = alpha, sd = (1 - alpha)^(1/3) * sd_b )))
    # Const <- sum(wt)
    dens_ai <- sapply(
      hw, 
      \(h) integrate(
        \(alpha) dnorm(h, mean = alpha, sd = (1 - alpha)^(1/3) * sd_b) * dnorm(alpha, mean = mu, sd = tau), 
        lower = 0, upper = 1 - 1e-6
      )$value
    )
    Const <- diff(pnorm(c(0,1), mean = mu, sd = tau))
  }
  if (log) {
    log(dens_ai) + log(jac_const) - log(Const)
  } else {
    dens_ai * jac_const / Const
  }
}

dat_hw <- 
  dat_b %>%
  mutate(
    mod_hw = map_dfr(1:k, loo_fit, mod = res_hw)
  ) %>%
  unnest(mod_hw, names_sep = "_") %>%
  rowwise() %>%
  mutate(
    lpd_hw = d_hw(
      ai = a, Ni = n, qi = items, 
      mu = mod_hw_mu, tau = mod_hw_tau, 
      log = TRUE
    )
  ) %>%
  ungroup()

rearrange <- \(x) {
  y <- str_match(x, "mod_(a|b|hw)_(mu|tau)")
  paste(y[,3], y[,2], sep = "_")
}

dat_all_fits <- 
  dat_hw %>%
  rename_with(.fn = rearrange, .cols = starts_with("mod_"))

#-------------------------------------------------------------------------------
# Summarize fits

all_est <- 
  bind_rows(
    a = a_est,
    b = b_est,
    hw = hw_est,
    .id = "Transformation"
  )


lpd_dat <- 
  dat_all_fits %>%
  select(studyid, esid, n, items, a, matches("_(a|b|hw)")) %>%
  pivot_longer(
    matches("_(a|b|hw)$"), 
    names_to = c(".value","metric"), names_pattern = "(.+)_(.+)$"
  )

lpd_dat %>%
  summarise(
    LPD = mean(lpd),
    SE = sd(lpd) / sqrt(k),
    .by = metric
  )

dat_all_fits %>%
  mutate(
    b_hw_diff = lpd_b - lpd_hw,
    b_a_diff = lpd_b - lpd_a,
    a_hw_diff = lpd_a - lpd_hw
  ) %>%
  summarise(
    across(ends_with("_diff"), list(LPD = ~ mean(.x), SE = ~ sd(.x) / sqrt(k)))
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


ai <- seq(0, 0.995, 0.005)
dist_dat <- tibble(
  ai = ai,
  d_a = d_a(ai, Ni = Inf, qi = 8, mu = as.numeric(res_a$beta), tau = sqrt(res_a$tau2), points = 499),
  d_b = d_b(ai, Ni = Inf, qi = 8, mu = as.numeric(res_b$beta), tau = sqrt(res_b$tau2)),
  d_hw = d_hw(ai, Ni = Inf, qi = 8, mu = as.numeric(res_hw$beta), tau = sqrt(res_hw$tau2), points = 499),
) %>%
  pivot_longer(starts_with("d_"), names_to = "metric", values_to = "density", names_prefix = "d_")

ggplot(dist_dat) + 
  aes(x = ai, y = density, fill = metric, color = metric) + 
  geom_area(alpha = 0.5, position = "identity") + 
  scale_x_continuous(limits = c(0, 1), expand = expansion(0,0)) +
  scale_y_continuous(labels = \(x) formatC(x, format = "f", digits = 2)) + 
  scale_fill_bmj() + 
  scale_color_bmj() + 
  labs(
    x = expression(a[i]), 
    y = expression(d[alpha](a)),
    title = "Estimated parameter distributions"
  ) + 
  theme_minimal() + 
  theme(legend.position = "inside", legend.position.inside = c(0.1,0.8))

outliers <-
  dat_all_fits %>%
  mutate(
    hw_diff = lpd_b - lpd_hw,
    a_diff = lpd_b - lpd_a
  ) %>%
  filter(a_diff < -0.31 | a_diff > 0.31) %>%
  select(studyid, esid, a, n, items, starts_with("mu_"), starts_with("tau_")) %>%
  cross_join(tibble(ai = seq(0,0.995,0.005))) %>%
  rowwise() %>%
  mutate(
    studyid = paste("Study", studyid),
    dens_a = d_a(ai = ai, Ni = n, qi = items, mu = mu_a, tau = tau_a, points = 200),
    dens_b = d_b(ai = ai, Ni = n, qi = items, mu = mu_b, tau = tau_b),
    dens_hw = d_hw(ai = ai, Ni = n, qi = items, mu = mu_hw, tau = tau_hw, points = 200)
  ) %>%
  ungroup() %>%
  pivot_longer(
    matches("_(a|b|hw)$"), 
    names_to = c(".value","metric"), names_pattern = "(.+)_(.+)"
  )

ggplot(outliers) + 
  aes(x = ai, y = dens, color = metric, fill = metric) + 
  facet_wrap(~ studyid) + 
  geom_area(alpha = 0.5, position = position_identity()) +
  geom_vline(aes(xintercept = a)) + 
  scale_x_continuous(expand = expansion(0,0)) + 
  scale_y_continuous(expand = expansion(0,c(0,0.05))) + 
  theme_minimal()
