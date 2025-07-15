library(tidyverse)
library(SingleCaseES)
library(brms)
library(tidybayes)


#-------------------------------------------------------------------------------
# Data cleaning

data("BartonArwood", package = "scdhlm")

BA <- 
  BartonArwood %>%
  mutate(
    obs = row_number(),
    outcome = round(outcome),
    case = factor(case, levels = c("Sam", "Gerald", "Rich", "Jack", "Emma", "Kim"))
  ) %>%
  group_by(case) %>%
  mutate(
    time_from_intervention = ifelse(
      condition == "A",
      session - max(session[condition == "A"]),
      session - min(session[condition == "B"]) + 1
    ),
    sess_by_trt = case_when(
      condition == "A" ~ 0,
      condition == "B" ~ time_from_intervention
    )
  ) %>%
  ungroup()

phase_change <- 
  BA %>%
  group_by(case) %>%
  mutate(
    condition_lag = lag(condition),
    phase_change = lag(session) + 1 / 2
  ) %>%
  filter(condition != condition_lag) %>%
  select(case, phase_change)

#-------------------------------------------------------------------------------
# Fit models

mod_grid <- expand_grid(
  formula = list(
    M1 = bf(outcome ~ condition + (1|case)),
    M2 = bf(outcome ~ condition + session + sess_by_trt + (1 + session + sess_by_trt | case))
  ),
  family = c("gaussian","poisson","negbinomial")
) %>%
  mutate(
    model_name = names(formula)
  )

set.seed(20250515)

mod_fits <- 
  mod_grid %>%
  mutate(
    model = map2(
      formula, family, 
      ~ brm(
        .x, data = BA, family = .y,
        control = list(adapt_delta = 0.96),
        save_pars = save_pars(all = TRUE),
        cores = 4
      )
    )
  )

#-------------------------------------------------------------------------------
# Extract LOO-ICs and generate predicted draws

# loo_vals <- 
#   mod_fits %>%
#   mutate(
#     loo = map(model, ~ as.tibble(loo(.x, moment_match = TRUE, reloo = TRUE)$estimates)[3,]),
#   ) %>%
#   select(-formula, -model) %>%
#   unnest(loo)

same_case_pred <- 
  mod_fits %>%
  mutate(
    pred = map(model, ~ add_predicted_draws(.x, newdata = BA, ndraws = 500))
  ) %>%
  select(-formula, -model) %>%
  unnest(pred) %>%
  select(family, model_name, .draw, case, condition, session, outcome = .prediction)

BA_new <-
  BA %>%
  mutate(
    case = fct_relabel(case, ~ paste0(.x, "*"))
  )

new_case_pred <- 
  mod_fits %>%
  mutate(
    pred = map(model, ~ add_predicted_draws(.x, newdata = BA_new, ndraws = 1000, allow_new_levels = TRUE))
  ) %>%
  select(-formula, -model) %>%
  unnest(pred) %>%
  select(-outcome) %>%
  select(family, model_name, .draw, case, condition, session, outcome = .prediction)

BA_RCT <- 
  expand_grid(
    group = c("A","B"),
    case = paste0("C",1:50),
    session = c(1,15),
  ) %>%
  mutate(
    case = paste0(group, case),
    condition = if_else(session == 1, "A", group),
    sess_by_trt = if_else(group == "A", 0, session - 1),
    outcome = NA
  )

RCT_pred <- 
  mod_fits %>%
  mutate(
    pred = map(model, ~ add_predicted_draws(.x, newdata = BA_RCT, ndraws = 100, allow_new_levels = TRUE))
  ) %>%
  select(-formula, -model) %>%
  unnest(pred) %>%
  select(-outcome) %>%
  select(family, model_name, .draw, group, case, session, outcome = .prediction) %>%
  pivot_wider(names_from = session, values_from = outcome) %>%
  rename(pretest = `1`, posttest = `15`)
  
#-------------------------------------------------------------------------------
# Calculate summary statistics for real and simulated data

# get unique set of families and models
family_models <- 
  same_case_pred %>%
  group_by(family, model_name) %>%
  summarize(.groups = "drop")

# initial level, mean level, trend, variability

BA_stats <- 
  BA %>%
  group_by(case, condition) %>%
  summarize(
    initial = outcome[row_number()==1],
    level = mean(outcome),
    sd = sd(outcome),
    PZD = mean(outcome <= 0),
    lm_fit = list(lm(outcome ~ session)),
    .groups = "drop"
  ) %>%
  mutate(
    slope = map_dbl(lm_fit, ~ coef(.x)[2]),
    sd_e = map_dbl(lm_fit, ~ summary(.x)$sigma)
  ) %>%
  select(-lm_fit) %>%
  cross_join(family_models) %>%
  mutate(.draw = 0L)

same_case_stats <- 
  same_case_pred %>%
  group_by(family, model_name, .draw, case, condition) %>%
  summarize(
    initial = outcome[row_number()==1],
    level = mean(outcome),
    sd = sd(outcome),
    PZD = mean(outcome <= 0),
    lm_fit = list(lm(outcome ~ session)),
    .groups = "drop"
  ) %>%
  mutate(
    slope = map_dbl(lm_fit, ~ coef(.x)[2]),
    sd_e = map_dbl(lm_fit, ~ summary(.x)$sigma)
  ) %>%
  select(-lm_fit) %>%
  bind_rows(BA_stats)

new_case_stats <- 
  new_case_pred %>%
  group_by(family, model_name, .draw, case, condition) %>%
  summarize(
    initial = outcome[row_number()==1],
    level = mean(outcome),
    sd = sd(outcome),
    PZD = mean(outcome <= 0),
    lm_fit = list(lm(outcome ~ session)),
    .groups = "drop"
  ) %>%
  mutate(
    slope = map_dbl(lm_fit, ~ coef(.x)[2]),
    sd_e = map_dbl(lm_fit, ~ summary(.x)$sigma)
  ) %>%
  select(-lm_fit)

# non-overlap

BA_nap <- batch_calc_ES(
  BA,
  grouping = case,
  condition = condition,
  outcome = outcome,
  ES = "NAP"
) %>%
  cross_join(family_models) %>%
  mutate(.draw = 0L)

same_case_nap <- batch_calc_ES(
  same_case_pred,
  grouping = c(family, model_name, .draw, case),
  condition = condition,
  outcome = outcome,
  ES = "NAP"
) %>%
  bind_rows(BA_nap)

# auto-correlation

calc_autocor <- function(data) {
  require(nlme)
  ac <- gls(
    outcome ~ case + case:session + case:condition + case:session:condition,
    data = data,
    correlation = corAR1(value = 0, form = ~ session | case)
  )
  
  as.numeric(coef(ac$modelStruct$corStruct, unconstrained = FALSE))
}

BA_auto <- 
  tibble(AR1 = calc_autocor(BA)) %>%
  cross_join(family_models) %>%
  mutate(.draw = 0L)

same_case_auto <- 
  same_case_pred %>%
  group_by(family, model_name, .draw) %>%
  summarize(
    AR1 = possibly(calc_autocor, otherwise = NA_real_)(pick(everything())),
    .groups = "drop"
  ) %>%
  bind_rows(BA_auto)


#-------------------------------------------------------------------------------
# Save

BA$.draw <- 0L

save(
  BA, phase_change, 
  mod_fits, 
  # loo_vals, 
  same_case_pred, new_case_pred, RCT_pred,
  BA_stats, BA_nap, BA_auto,
  same_case_stats, same_case_nap, same_case_auto,
  new_case_stats, 
  file = "WISCC 2025/BA-example.Rdata"
)
