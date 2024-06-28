library(tidyverse)
library(future)
library(furrr)

check_boot_CI <- function(k, B, mu, scale) {
  dat <- rgamma(k, shape = mu / scale, scale = 1.6)
  xbar_trim <- mean(dat, trim = 0.1)
  booties <- replicate(max(B), mean(sample(dat, size = k, replace = TRUE), trim = 0.1))
  CIs <- map(B, ~ quantile(booties[1:.x], c(.025, .975)))
  CI_lo <- map_dbl(CIs, ~ .x[1])
  CI_hi <- map_dbl(CIs, ~ .x[2])
  covers <- CI_lo < mu & mu < CI_hi
  data.frame(B_sub = B, CI_lo, CI_hi, covers)
}

bootstrap_coverage <- function(reps, k, B, mu, scale) {
  res <- replicate(
    reps, 
    check_boot_CI(k = k, B = B, mu = mu, scale = scale), 
    simplify = FALSE
  )
  
  bind_rows(res) %>%
    group_by(B_sub) %>%
    summarise(
      coverage = mean(covers),
      width = mean(CI_hi - CI_lo)
    )
}

reps <- 100
k <- 100
B <- 99
mu <- 6
scale <- 1.6
bootstrap_coverage(reps, k, B, mu, scale)
bootstrap_coverage(reps, k, B = c(39,59,99), mu, scale)

plan(multisession)

B_vals <- c(39, 59, 79, 99, 199, 399, 799, 1199)
boot_experiments <- 
  tibble(
    B = map(B_vals, ~ B_vals[B_vals <= .x]),
    B_grp = B_vals, 
    B_inverse = 1 / B_vals
  ) %>%
  filter(B_vals > 39) %>%
  mutate(
    res = future_map(B, bootstrap_coverage, reps = 2000, k = 80, mu = 6, scale = 1.6, .options = furrr_options(seed = TRUE))
  ) %>%
  unnest(res)

boot_experiments <- 
  boot_experiments %>%
  filter(B_grp == B_sub) %>%
  mutate(B_grp = NA_real_) %>%
  bind_rows(boot_experiments) %>%
  mutate(B = fct_explicit_na(factor(B_grp), na_level = "Independent")) %>%
  select(-B_grp)
  
boot_experiments_long <- 
  boot_experiments%>%
  pivot_longer(c(coverage, width), names_to = "q", values_to = "val")

ggplot(boot_experiments_long, aes(B_sub, val, color = B)) + 
  geom_point(size = 2) + geom_line() + 
  scale_x_continuous(breaks = B_vals, transform  = "reciprocal") + 
  geom_smooth(
    data = filter(boot_experiments_long, B_sub <= 399),
    method = "lm", formula = y ~ x, 
    se = FALSE, fullrange = TRUE
  ) + 
  facet_wrap(~ q, scales = "free") + 
  expand_limits(x = 1e5) + 
  theme_minimal() + 
  labs(x = "1 / B", y = "", color = "B")

ggplot(boot_experiments, aes(B_sub, coverage, color = B)) + 
  geom_point(size = 2) + geom_line() + 
  scale_x_continuous(breaks = B_vals, transform  = "reciprocal") + 
  geom_smooth(
    data = filter(boot_experiments, B_sub <= 399),
    method = "lm", formula = y ~ x, 
    se = FALSE, fullrange = TRUE
  ) + 
  facet_wrap(~ B, ncol = 2, scales = "fixed", labeller = "label_both") + 
  expand_limits(x = 1e5) + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  labs(x = "1 / B", y = "")

ggplot(boot_experiments, aes(B_sub, coverage, color = B)) + 
  geom_point(size = 2) + geom_line() + 
  scale_x_continuous(breaks = B_vals, transform  = "reciprocal") + 
  geom_smooth(
    data = filter(boot_experiments, B_sub <= 399),
    method = "lm", formula = y ~ poly(x,2), 
    se = FALSE, fullrange = TRUE
  ) + 
  facet_wrap(~ B, ncol = 2, scales = "fixed", labeller = "label_both") + 
  expand_limits(x = 1e5) + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  labs(x = "1 / B", y = "")

ggplot(boot_experiments, aes(B_sub, width, color = B)) + 
  geom_point(size = 2) + geom_line() + 
  scale_x_continuous(breaks = B_vals, transform  = "reciprocal") + 
  geom_smooth(
    data = filter(boot_experiments, B_sub <= 399),
    method = "lm", formula = y ~ x, 
    se = FALSE, fullrange = TRUE
  ) + 
  facet_wrap(~ B, ncol = 2, scales = "fixed", labeller = "label_both") + 
  expand_limits(x = 1e5) + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  labs(x = "1 / B", y = "")
