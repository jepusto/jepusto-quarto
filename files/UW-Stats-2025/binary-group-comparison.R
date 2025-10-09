library(conflicted)
library(tidyverse)
conflicts_prefer(dplyr::filter, .quiet = TRUE)

library(metadat)
library(metafor)
library(statmod)
library(VGAM)
library(future)
library(furrr)
plan(multisession)

data("dat.hartmannboyce2018", package = "metadat")

dat <- 
  dat.hartmannboyce2018 %>%
  mutate(
    Ni = n.ctrl + n.nrt,
    studyid = row_number(),
    p_C = x.ctrl / n.ctrl
  ) %>%
  escalc(measure="RR", ai=x.nrt, n1i=n.nrt, ci=x.ctrl, n2i=n.ctrl, data = ., var.names = c("RR", "V_RR")) %>%
  escalc(measure="RR", ai=n.ctrl - x.ctrl, n1i=n.ctrl, ci=n.nrt - x.nrt, n2i=n.nrt, data = ., var.names = c("cRR", "V_cRR")) %>%
  escalc(measure="OR", ai=x.nrt, n1i=n.nrt, ci=x.ctrl, n2i=n.ctrl, data = ., var.names = c("OR", "V_OR")) %>%
  escalc(measure="RD", ai=x.nrt, n1i=n.nrt, ci=x.ctrl, n2i=n.ctrl, data = ., var.names = c("RD", "V_RD"))

k <- nrow(dat)
n_summary <- summary(dat$Ni)

#-------------------------------------------------------------------------------
# Model for control group risk

ctrl_fit <- function(data, i = 0) {
  id <- 1:nrow(dat)
  mod_i <- vglm(
    cbind(x.ctrl, n.ctrl - x.ctrl) ~ 1, 
    family = betabinomial, 
    data = data, 
    subset = id != i,
    trace = FALSE
  )
  coef_i <- Coef(mod_i)
  data.frame(
    pi = coef_i[["mu"]],
    rho = coef_i[["rho"]]
  )
}

ctrl_fit(data = dat)

  
#-------------------------------------------------------------------------------
# Meta-analysis models

effect_fit <- function(data, yi, vi, i = 0, return_mod = FALSE) {
  cl <- match.call()
  m <- match(c("yi","vi","data"), names(cl))
  rma_call <- cl[c(1L,m)]
  id <- 1:nrow(dat)
  rma_call$subset <- id != i
  rma_call[[1]] <- quote(rma.uni)
  mod_i <- eval(rma_call, parent.frame())
  
  if (return_mod) return(mod_i) 
  
  data.frame(
    mu = as.numeric(mod_i$b),
    tau = sqrt(mod_i$tau2),
    I2 = mod_i$I2
  )
}

CI_PI <- function(mod,..., CI_level = 95, PI_level = 80) {
  CI_res <- 
    predict(mod, ..., level = CI_level) %>%
    as.data.frame() %>%
    remove_rownames() %>%
    mutate(
      mu = as.numeric(mod$b),
      tau = sqrt(mod$tau2)
    )
  
  PI_res <- predict(mod, ..., level = PI_level) %>%
    as.data.frame() %>%
    remove_rownames() %>%
    mutate(I2 = mod$I2)
  
  CI_res %>%
    select(mu, tau, Est = pred, ci.lb, ci.ub) %>%
    bind_cols(select(PI_res, pi.lb, pi.ub, I2))
}

RE_summary <- 
  bind_rows(
    RD = CI_PI(effect_fit(dat = dat, yi = RD, vi = V_RD, return_mod = TRUE)),
    RR = CI_PI(effect_fit(dat = dat, yi = RR, vi = V_RR, return_mod = TRUE)),
    cRR = CI_PI(effect_fit(dat = dat, yi = cRR, vi = V_cRR, return_mod = TRUE)),
    OR = CI_PI(effect_fit(dat = dat, yi = OR, vi = V_OR, return_mod = TRUE)),
    .id = "Metric"
  )

RE_summary %>%
  select(-mu, -tau) %>%
  mutate(
    across(-Metric, ~ formatC(., digits = 2, format = "f"))
  ) %>%
  unite("95% CI", starts_with("ci."), sep = "-") %>%
  unite("80% PI", starts_with("pi."), sep = "-")

conditional_PIs <- 
  RE_summary %>%
  select(Metric, mu, pi.lb, pi.ub) %>%
  cross_join(tibble(pi_C = seq(0,0.5,0.005))) %>%
  mutate(
    across(
      c(mu, pi.lb, pi.ub),
      ~ case_match(
        Metric,
        "RD" ~ pi_C + .x,
        "RR" ~ exp(log(pi_C) + .x),
        "cRR" ~ 1 - exp(log(1 - pi_C) - .x),
        "OR" ~ plogis(qlogis(pi_C) + .x)
      )
    )
  ) %>%
  mutate(
    Metric = factor(Metric, levels = c("RD","RR","cRR","OR"))
  )

ggplot(conditional_PIs) + 
  aes(x = pi_C, y = mu, ymin = pi.lb, ymax = pi.ub, fill = Metric, color = Metric) + 
  geom_hline(yintercept = 0) + 
  geom_line() +
  geom_ribbon(alpha = 0.3) +
  scale_x_continuous(limits = c(0, 0.5), expand = expansion(0,0)) + 
  labs(
    x = expression(pi[0]),
    y = expression(pi[1]),
    title = "80% Prediction intervals for abstinence probability with NRT",
    subtitle = "Given abstinence probability under control"
  ) + 
  theme(legend.position = "inside", legend.position.inside = c(0.15,0.8))

#-------------------------------------------------------------------------------
# Leave-one-out calculations

dat_loo <- 
  dat %>%
  mutate(
    fit_ctrl = future_map_dfr(1:k, ctrl_fit, data = dat, .progress = TRUE),
    fit_OR = future_map_dfr(1:k, effect_fit, data = dat, yi = OR, vi = V_OR, .progress = TRUE),
    fit_RR = future_map_dfr(1:k, effect_fit, data = dat, yi = RR, vi = V_RR, .progress = TRUE),
    fit_cRR = future_map_dfr(1:k, effect_fit, data = dat, yi = cRR, vi = V_cRR, .progress = TRUE),
    fit_RD = future_map_dfr(1:k, effect_fit, data = dat, yi = RD, vi = V_RD, .progress = TRUE)
  ) %>%
  unnest(starts_with("fit_"), names_sep = "_")

#-------------------------------------------------------------------------------
# LPD calculations

lpd <- function(ai, n1i, ci, n2i, pi, rho, mu, tau, transf, itransf, pts_ctl = 49, pts_theta = 49, log = TRUE) {
  
  alpha <- pi * (1 - rho) / rho
  beta <- (1 - pi) * (1 - rho) / rho
  qp_0 <- gauss.quad.prob(n = pts_ctl, dist = "beta", alpha = alpha, beta = beta)
  qp_1 <- gauss.quad.prob(n = pts_theta, dist = "normal", mu = mu, sigma = tau)
  qp <- 
    cross_join(
      as.data.frame(qp_0), 
      as.data.frame(qp_1),
    ) %>%
    mutate(
      lwt = log(weights.x) + log(weights.y),
      pi0 = nodes.x,
      pi1 = itransf(transf(nodes.x) + nodes.y)
    ) %>%
    filter(0 < pi1, pi1 < 1)
  
  dens <- pmap_dbl(
    list(x1 = ai, n1 = n1i, x0 = ci, n0 = n2i), 
    \(x1, n1, x0, n0) sum(exp(dbinom(x = x0, size = n0, prob = qp$pi0, log = TRUE) + dbinom(x = x1, size = n1, prob = qp$pi1, log = TRUE) + qp$lwt))
  )
  
  if (log) {
    log(dens) - log(sum(exp(qp$lwt)))
  } else {
    dens / sum(exp(qp$lwt))
  }
  
}

dat_lpd <- 
  dat_loo %>%
  rowwise() %>%
  mutate(
    lpd_OR = lpd(
      ai = x.nrt, n1i = n.nrt, ci = x.ctrl, n2i = n.ctrl, 
      pi = fit_ctrl_pi, rho = fit_ctrl_rho, 
      mu = fit_OR_mu, tau = fit_OR_tau, transf = qlogis, itransf = plogis
    ),
    lpd_RR = lpd(
      ai = x.nrt, n1i = n.nrt, ci = x.ctrl, n2i = n.ctrl, 
      pi = fit_ctrl_pi, rho = fit_ctrl_rho, 
      mu = fit_RR_mu, tau = fit_RR_tau, transf = log, itransf = exp
    ),
    lpd_cRR = lpd(
      ai = x.nrt, n1i = n.nrt, ci = x.ctrl, n2i = n.ctrl, 
      pi = fit_ctrl_pi, rho = fit_ctrl_rho, 
      mu = fit_cRR_mu, tau = fit_cRR_tau, transf = \(x) -log(1 - x), itransf = \(x) 1 - exp(-x)
    ),
    lpd_RD = lpd(
      ai = x.nrt, n1i = n.nrt, ci = x.ctrl, n2i = n.ctrl, 
      pi = fit_ctrl_pi, rho = fit_ctrl_rho, 
      mu = fit_OR_mu, tau = fit_OR_tau, transf = identity, itransf = identity
    )
  ) %>% 
  ungroup()

dat_lpd_long <- 
  dat_lpd %>%
  select(study, studyid, starts_with("lpd_")) %>%
  pivot_longer(starts_with("lpd_"), names_to = "metric", names_prefix = "lpd_", values_to = "lpd") %>%
  mutate(
    metric = factor(metric, levels = c("OR","RR","cRR","RD"), labels = c("Odds ratio", "Risk ratio", "Complementary risk ratio", "Risk difference"))
  )

lpd_summary <- 
  dat_lpd_long %>%
  group_by(studyid) %>%
  mutate(
    diff_OR = lpd - lpd[metric == "Odds ratio"]
  ) %>%
  group_by(metric) %>%
  summarize(
    LPD = mean(lpd),
    SE = sd(lpd) / sqrt(k),
    LPD_diff_OR = mean(diff_OR),
    SE_diff = sd(diff_OR) / sqrt(k)
  ) %>%
  mutate(
    across(c(LPD_diff_OR, SE_diff), ~ if_else(metric == "Odds ratio", NA_real_, .x))
  )

lpd_summary
