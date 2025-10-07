library(conflicted)
library(tidyverse)
library(flextable)
conflicts_prefer(dplyr::filter, .quiet = TRUE)
conflicts_prefer(flextable::separate_header, .quiet = TRUE)

library(metadat)
library(metafor)

data("dat.crede2010", package = "metadat")
dat <- 
  dat.crede2010 %>%
  mutate(
    ri = if_else(ri < -0.1, -ri, ri) 
  ) %>%
  escalc(measure="ZCOR", ri=ri, ni=ni, data = ., var.names = c("z", "Vz")) %>%
  escalc(measure="COR", ri=ri, ni=ni, data = ., var.names = c("r", "Vr")) %>%
  mutate()

k <- nrow(dat)
#-------------------------------------------------------------------------------
# Pearson's r
res_r <- rma(yi = r, vi = Vr, data = dat)
summary(res_r)
funnel(res_r)

f_lpd_r <- function(i, mod) {
  id <- 1:mod$k
  mod_i <- update(mod, subset = id != i)
  sd_est <- sqrt(mod_i$tau2 + mod$vi[i])
  mean_est <- as.numeric(mod_i$b)
  dnorm(mod$yi[i], mean = mean_est, sd = sd_est, log = TRUE)
}

dat$lpd_r <- map_dbl(1:k, f_lpd_r, mod = res_r)
summary(dat$lpd_r)
sum(dat$lpd_Z)

#-------------------------------------------------------------------------------
# Fisher's z

res_Z <- rma(yi = z, vi = Vz, data = dat)
summary(res_Z)
funnel(res_Z)

f_lpd_Z <- function(i, mod) {
  id <- 1:mod$k
  mod_i <- update(mod, subset = id != i)
  sd_est <- sqrt(mod_i$tau2 + mod$vi[i])
  mean_est <- as.numeric(mod_i$b)
  dnorm(mod$yi[i], mean = mean_est, sd = sd_est, log = TRUE)
}

dat$lpd_Z <- map_dbl(1:k, f_lpd_Z, mod = res_Z)
summary(dat$lpd_Z)
sum(dat$lpd_Z)
