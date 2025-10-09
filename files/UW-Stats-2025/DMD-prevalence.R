library(conflicted)
library(tidyverse)
conflicts_prefer(dplyr::filter, .quiet = TRUE)

library(metadat)
library(metafor)
library(statmod)
library(lme4)

data("dat.crisafulli2020", package = "metadat")

dat <- 
  dat.crisafulli2020 %>%
  escalc(measure="PR", xi=cases, ni=total, data=., var.names = c("pr", "Vpr"))

k <- nrow(dat)
n_summary <- summary(dat$total)


ggplot(dat) + 
  aes(pr, 1 / sqrt(total)) + 
  geom_hline(yintercept = 0) + 
  geom_point(size = 2, color = "darkred") +
  scale_x_continuous(limits = c(1e-4, NA), transform = "log10") + 
  scale_y_reverse(expand = expansion(0, c(0.001,0))) + 
  labs(
    x = expression(p[i]),
    y = expression(1 / sqrt(N[i]))
  ) + 
  theme_minimal()


#-------------------------------------------------------------------------------
# Transformations

loo_fit <- function(dat, i) {
  id <- 1:nrow(dat)
  mod_i <- rma(yi = yi, vi = vi, data = dat, subset = id != i)
  data.frame(
    mu = as.numeric(mod_i$b),
    tau = sqrt(mod_i$tau2)
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

normal_interval <- function(theta, es, Ni, mu, tau, transf, itransf, deriv) {
  l_u <- transf(itransf(es) + c(-0.5, 0.5) / Ni)
  pi <- itransf(theta)
  gp <- deriv(pi)
  sd <- sqrt(pi * (1 - pi) * gp^2 / Ni)
  (pnorm(l_u[2], mean = theta, sd = sd) - pnorm(l_u[1], mean = theta, sd = sd)) * dnorm(theta, mean = mu, sd = tau)
}

binomial_mass <- function(theta, es, Ni, mu, tau, transf, itransf, ...) {
  xi <- itransf(es) * Ni
  pi <- itransf(theta)
  dbinom(xi, size = Ni, prob = pi) * dnorm(theta, mean = mu, sd = tau)
}

d_trans <- function(pi, Ni, mu, tau, transf, itransf, deriv, integrand = normal_interval, log = FALSE) {
  
  es_i <- transf(pi)
  theta_range <- transf(c(0,1))
  Const <- diff(pnorm(theta_range, mean = mu, sd = tau))
  lower <- max(theta_range[1], mu - 10 * tau)
  upper <- min(theta_range[2], mu + 10 * tau)
  
  if (is.infinite(Ni)) {
    dens_ai <- dnorm(es_i, mean = mu, sd = tau) * deriv(pi)
  } else {
    dens_ai <- sapply(
      es_i, 
      \(es) integrate(
        integrand, lower = lower, upper = upper, 
        es = es, Ni = Ni, mu = mu, tau = tau,
        transf = transf, itransf = itransf, deriv = deriv
      )$value
    )
  }
  if (log) {
    log(dens_ai) - log(Const)
  } else {
    dens_ai / Const
  }
}

eval_model <- function(dat, measure, transf, itransf, deriv) {
  
  k <- nrow(dat)
  es_dat <- escalc(measure = measure, xi = cases, ni = total, data = dat)
  
  pi <- dat$cases / dat$total
  yi <- transf(pi)
  vi <- deriv(pi)^2 * pi * (1 - pi) / dat$total
  if (!isTRUE(all.equal(yi, es_dat$yi, check.attributes = FALSE))) stop("Effect sizes are not equal to the transformed probabilities.")
  if (!isTRUE(all.equal(vi, es_dat$vi))) stop("Delta-method variances are not equal to the escalc results.")
  
  full_mod <- rma(yi = yi, vi = vi, data = es_dat)
  res <- CI_PI(full_mod, transf = itransf)
  
  loo_dat <- 
    es_dat %>%
    mutate(
      mod = map_dfr(1:k, loo_fit, dat = es_dat)
    ) %>%
    unnest(mod, names_sep = "_") %>%
    rowwise() %>%
    mutate(
      lpd_trans = d_trans(
        pi = pr, Ni = total, mu = mod_mu, tau = mod_tau,
        transf = transf, itransf = itransf, deriv = deriv,
        integrand = normal_interval, log = TRUE
      ),
      lpd_binom = d_trans(
        pi = pr, Ni = total, mu = mod_mu, tau = mod_tau,
        transf = transf, itransf = itransf, deriv = deriv,
        integrand = binomial_mass, log = TRUE
      )
    ) %>%
    ungroup()

  lpd_summary <- 
    loo_dat %>%
    summarize(
      LPD_trans = mean(lpd_trans),
      SE_trans = sd(lpd_trans) / sqrt(k),
      LPD_trans_i = list(lpd_trans),
      LPD_binom = mean(lpd_binom),
      SE_binom = sd(lpd_binom) / sqrt(k),
      LPD_binom_i = list(lpd_binom)
    )
  
  return(bind_cols(res, lpd_summary))
}

log_res <- 
  eval_model(
    dat = dat, 
    measure = "PLN", 
    transf = log, itransf = exp, 
    deriv = \(x) 1 / x
  )

logit_res <- 
  eval_model(
    dat = dat, 
    measure = "PLO", 
    transf = qlogis, itransf = plogis, 
    deriv = \(x) 1 / (x * (1 - x))
  )

probit_res <- 
  eval_model(
    dat = dat, 
    measure = "PRZ", 
    transf = qnorm, itransf = pnorm, 
    deriv = \(x) 1 / dnorm(qnorm(x))
  )

arcsin_res <- 
  eval_model(
    dat = dat, 
    measure = "PAS", 
    transf = transf.arcsin, itransf = transf.iarcsin, 
    deriv = \(x) 1 / (2 * sqrt(x * (1 - x)))
  )


#-------------------------------------------------------------------------------
# GLMMs

fit_glmr <- function(dat, link = "logit", i = 0) {
  id <- 1:nrow(dat)
  mod_i <- glmer(
    cbind(cases, total - cases) ~ 1 | study, 
    data = dat,
    family = binomial(link = link), 
    subset = id != i,
    nAGQ = 9
  )
  data.frame(
    mu = fixef(mod_i),
    mu_se = sqrt(as.numeric(vcov(mod_i))),
    tau = sqrt(summary(mod_i)$varcor$study[[1]])
  )
}

glmm_CI_PI <- function(mod, itransf = identity, CI_level = 95, PI_level = 80) {
  q_CI <- qnorm((100 + CI_level) / 200)
  q_PI <- qnorm((100 + PI_level) / 200)
  sd_PI <- sqrt(mod$tau^2 + mod$mu_se^2)
  data.frame(
    mu = mod$mu,
    tau = mod$tau,
    Est = itransf(mod$mu),
    ci.lb = itransf(mod$mu - q_CI * mod$mu_se),
    ci.ub = itransf(mod$mu + q_CI * mod$mu_se),
    pi.lb = itransf(mod$mu - q_PI * sd_PI),
    pi.ub = itransf(mod$mu + q_PI * sd_PI)
    
  )
}

# logit link 

full_GLMM_logit <- fit_glmr(dat = dat)

loo_logit <- 
  dat %>%
  mutate(
    mod = map_dfr(1:k, fit_glmr, link = "logit", dat = dat)
  ) %>%
  unnest(mod, names_sep = "_") %>%
  rowwise() %>%
  mutate(
    lpd_binom = d_trans(
      pi = pr, Ni = total, mu = mod_mu, tau = mod_tau,
      transf = qlogis, itransf = plogis, deriv = NULL,
      integrand = binomial_mass, log = TRUE
    )
  ) %>%
  ungroup()

lpd_logit <- 
  loo_logit %>%
  summarize(
    LPD_binom = mean(lpd_binom),
    SE_binom = sd(lpd_binom) / sqrt(k),
    LPD_binom_i = list(lpd_binom)
  )

logit_GLMM_res <- 
  glmm_CI_PI(full_GLMM_logit, itransf = plogis) %>%
  bind_cols(lpd_logit)

# probit link 

full_GLMM_probit <- fit_glmr(dat = dat, link = "probit")

loo_probit <- 
  dat %>%
  mutate(
    mod = map_dfr(1:k, fit_glmr, link = "probit", dat = dat)
  ) %>%
  unnest(mod, names_sep = "_") %>%
  rowwise() %>%
  mutate(
    lpd_binom = d_trans(
      pi = pr, Ni = total, mu = mod_mu, tau = mod_tau,
      transf = qnorm, itransf = pnorm, deriv = NULL,
      integrand = binomial_mass, log = TRUE
    )
  ) %>%
  ungroup()

lpd_probit <- 
  loo_probit %>%
  summarize(
    LPD_binom = mean(lpd_binom),
    SE_binom = sd(lpd_binom) / sqrt(k),
    LPD_binom_i = list(lpd_binom)
  )

probit_GLMM_res <- 
  glmm_CI_PI(full_GLMM_probit, itransf = pnorm) %>%
  bind_cols(lpd_probit)


all_res <- bind_rows(
  # `log-RE` = log_res,
  `logit-RE` = logit_res,
  `probit-RE` = probit_res,
  `arcsin-RE` = arcsin_res,
  `logit-GLMM` = logit_GLMM_res,
  `probit-GLMM` = probit_GLMM_res,
  .id = "measure"
)

all_res %>%
  select(-ends_with("_i"))

lpd_dat <-
  all_res %>%
  select(measure, ends_with("_i")) %>%
  unnest(c(LPD_trans_i, LPD_binom_i))

ggplot(lpd_dat) + 
  aes(LPD_binom_i, color = measure, fill = measure) + 
  geom_density(alpha = 0.5) +
  geom_rug() + 
  labs(
    x = "log predictive density contribution",
    y = ""
  ) + 
  theme_minimal()

