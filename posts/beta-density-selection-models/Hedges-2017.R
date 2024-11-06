library(tidyverse)

Baskerville <- tribble(
  ~ Ti, ~ V, ~ Blinded, ~ Ni, ~ Ni_blinded,
  1.01,	0.2704,	'B',  18.16,    8.168,
  0.82,	0.2116,	'O',  22.64,       NA,
  0.59,	0.0529,	'O',   7.50,       NA,
  0.44,	0.0324,	'O',   8.96,       NA, 
  0.84,	0.0841,	'B',   4.49,    1.533,
  0.73,	0.0841,	'O',   8.05,       NA,
  1.12,	0.1296,	'B',   3.14,    1.000,
  0.04,	0.1369,	'B', 587.57,  471.339,
  0.24,	0.0225,	'O',  29.25,       NA,
  0.32,	0.1600,	'O', 108.63,       NA,
  1.04,	0.1024,	'O',   2.47,       NA,
  1.31,	0.3249,	'B',  11.06,    4.519,
  0.59,	0.0841,	'B',  15.99,    7.019,
  0.66,	0.0361,	'O',   1.65,       NA,
  0.62,	0.0961,	'B',  16.77,    7.429,
  0.47,	0.0729,	'B',  24.00,   11.377,
  1.08,	0.1024,	'O',   1.97,       NA,
  0.98,	0.1024,	'B',   3.41,    1.104,
  0.26,	0.0324,	'B',  36.64,   18.772,
  0.39,	0.0324,	'B',  13.31,    5.638,
  0.60,	0.0961,	'B',  18.33,    8.260,
  0.94,	0.2809,	'B',  22.93,   10.777,
  0.11,	0.0729,	'B', 259.32,  183.557
)

ll <- function(y, se, mu, tausq, lambda) {
  p <- pnorm(-y / se)
  wt <- p^(lambda[1] - 1) * (1 - p)^(lambda[2] - 1)
  eta <- sqrt(tausq + se^2)
  norm_dens <- dnorm((y - mu) / eta) / eta
  norm_dens * wt
}

calc_gi_T <- function(se, mu, tausq, lambda, alpha = 1e-5) {
  b <- qnorm(alpha) * se
  Ai <- integrate(ll, lower = b, upper = -b, se = se, mu = mu, tausq = tausq, lambda = lambda)
  return(Ai$value)
}

mu <- 0.115
tausq <- 0
lambda <- c(0.473, 4.459)

Table1 <- 
  Baskerville %>%
  mutate(
    Study = 1:n(),
    SEi = sqrt(V),
    p = pnorm(Ti / SEi, lower.tail = FALSE),
    wp = (p^(lambda[1] - 1)) * (1 - p)^(lambda[2] - 1),
    wp_scaled = wp / wp[14],
    gi_T = map_dbl(SEi, .f = calc_gi_T, mu = mu, tausq = tausq, lambda = lambda),
    Ni_calc = wp[14] / gi_T,
    ratio = Ni / Ni_calc
  ) %>%
  select(
    Study, Ti, SEi, p, wp, wp_scaled, 
    gi_T, Ni_calc, Ni, ratio
  )

Table1 %>%
  arrange(SEi) %>%
  View()

Table1 %>%
  summarize(
    N = sum(Ni),
    k_total = sum(Ni_calc)
  )
