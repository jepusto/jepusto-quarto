m <- 6L
ESS <- 80
mu <- 0.3
tau <- 0.1
omega <- 0.1
rho <- 0.6
alpha <- 0.025
qp <- 21L

K <- 1e6

sigma <- 2 / sqrt(ESS)
zeta_sd <- sqrt(tau^2 + rho * sigma^2)
ID_sd <- sqrt(omega^2 + (1 - rho) * sigma^2)
zeta <- rnorm(K, mean = mu, sd = zeta_sd)
psi <- pnorm((mu - crit * sigma) / sqrt(tau^2 + omega^2 + sigma^2))
crit <- qnorm(1 - alpha)

pi_sd_approx <- dnorm((mu - crit * sigma) / ID_sd) * zeta_sd / ID_sd
V_approx <- m * psi * (1 - psi) + m * (m - 1) * pi_sd_approx^2

pi_samp <- pnorm((zeta - crit * sigma) / ID_sd)
mean(pi_samp)
psi
sd(pi_samp)
dnorm((mu - crit * sigma) / ID_sd) * zeta_sd / ID_sd

NA_samp <- rbinom(K, size = m, prob = pi_samp)
mean(NA_samp)
psi * m
var(NA_samp)
V_approx
