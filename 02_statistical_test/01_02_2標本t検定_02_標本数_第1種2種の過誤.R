rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# set paramters
# ------------------------------------------------------------------------------

beta <- 0.2


( Zb <- qnorm(p = beta, mean = 0, sd = 1) )


alpha <- 0.05


( Z2a <- qnorm(p = 1 - alpha / 2, mean = 0, sd = 1) )


sd_common <- 0.5


effect_size <- sd_common * 0.2



# ------------------------------------------------------------------------------
# estimate sample size
# ------------------------------------------------------------------------------

( n <- ceiling(2 * (Z2a - Zb) ^ 2 * sd_common ^ 2 / (effect_size ^ 2) ) )




# ------------------------------------------------------------------------------
# effect size validation
# ------------------------------------------------------------------------------


( A <- Z2a * sqrt(2) * sd_common / sqrt(n) )

( B <- Zb * sqrt(2) * sd_common / sqrt(n) )


( A - B )


effect_size




# ------------------------------------------------------------------------------
# mu of h0 and h1
# ------------------------------------------------------------------------------

mu_h0 <- 5.0



( mu_h1 <- mu_h0 + effect_size )


# ( mu_h12 <- ( Z2a * sqrt(2) * sd_common + Zb * sqrt(2) * sd_common ) / sqrt(n) )




# ------------------------------------------------------------------------------
# simulation of one sample
# ------------------------------------------------------------------------------

dat_h0 <- rnorm(n = n, mean = mu_h0, sd = sd_common)

dat_h1 <- rnorm(n = n, mean = mu_h1, sd = sd_common)



# ----------
min_all <- min(c(dat_h0, dat_h1))

max_all <- max(c(dat_h0, dat_h1))



par(mfrow = c(2,1))

brk <- seq(min_all - 0.1, max_all + 0.1, by = 0.1)


hist(dat_h0, breaks = brk)
abline(v = mu_h0, lty = 1, col = c("black"), lwd = 2)


hist(dat_h1, breaks = brk)
abline(v = mu_h1, lty = 1, col = c("black"), lwd = 2)




# ------------------------------------------------------------------------------
# distribution of difference of means
# ------------------------------------------------------------------------------

myfunc <- function(n, mean, sd){
  
  dat <- rnorm(n = n, mean = mean, sd = sd)
  
  return(mean(dat))
}



# group 1
dat_h0_g1_mean <- sapply(1:10000, function(x) { myfunc(n = n, mean = mu_h0, sd = sd_common) })


# group 2 at H0
dat_h0_g2_mean <- sapply(1:10000, function(x) { myfunc(n = n, mean = mu_h0, sd = sd_common) })


# group 2 at H1
dat_h1_g2_mean <- sapply(1:10000, function(x) { myfunc(n = n, mean = mu_h1, sd = sd_common) })



# ----------
# difference of means

diff_h0 <- dat_h0_g2_mean - dat_h0_g1_mean

diff_h1 <- dat_h1_g2_mean - dat_h0_g1_mean




# ----------
# standard deviation of differences
sd(diff_h0)

sd(diff_h1)

sqrt(2) * sd_common / sqrt(n)




# ----------
# H0:  alpha / 2
sum(diff_h0 > A) / length(diff_h0)


# H1:  beta
sum(diff_h1 < A) / length(diff_h1)


beta




# ----------
min_all <- min(c(diff_h0, diff_h1))

max_all <- max(c(diff_h0, diff_h1))


graphics.off()

par(mfrow = c(2,1))

brk <- seq(min_all - 0.1, max_all + 0.1, by = 0.01)


hist(diff_h0, breaks = brk, main = paste0("diff of means of H0, alpha/2 = ", round(alpha/2, 3)))
abline(v = c(0, A), lty = 1, col = c("black", "blue"), lwd = 2)


hist(diff_h1, breaks = brk, main = paste0("diff of means at H1, beta = ", round(beta, 3)))
abline(v = c(A, effect_size), lty = 1, col = c("blue", "black"), lwd = 2)

