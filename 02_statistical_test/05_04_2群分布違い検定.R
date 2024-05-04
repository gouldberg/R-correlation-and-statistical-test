rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

ksdata <- c(90, 126, 232.41, 94.5, 156, 120.9, 180.4, 32.25, 29.2, 100.11, 21, 23.1, 77, 78.75, 122.32, 59.78, 95.59,
            124.95, 112.8, 98.4, 31.39, 31.05, 26.4, 184, 72.8, 62.98, 23.56, 53.35, 49.5, 33.48, 99.33, 39.2, 11.34,
            31.92, 41.8)


ks2data <- c(94.4, 117, 230.4, 91, 166.32, 139.12, 206.48, 56.76, 29.6, 96.75, 19.2, 23.18, 104, 82.5, 115.2, 65.52,
             100.33, 113.9, 132.8, 96, 29.4, 49.92, 26.8, 149.73, 74.52, 61.36, 23.4, 39.22, 49.02, 49.02, 33, 102,
             33.44, 34.03, 34.79)


length(ksdata)

length(ks2data)


( df <- data.frame(dat = c(ksdata, ks2data), group = c(rep(1, length(ksdata)), rep(2, length(ks2data)))) )



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))


boxplot(dat ~ group, data = df)


lattice::histogram(~ dat | group, data = df)




# ------------------------------------------------------------------------------
# ECDF (Empirical Cumulative Distribution Function)
# ------------------------------------------------------------------------------

library(latticeExtra)


par(mfrow = c(1,1))

df2 <- data.frame(dat_g1 = ksdata, dat_g2 = ks2data)

ecdfplot(~ dat_g1 + dat_g2, data = df2, col = c("blue", "red"), auto.key = list(space = "right"))




# ------------------------------------------------------------------------------
# Kolmogorov-Smirnov normality test:  ks.test
# ------------------------------------------------------------------------------

# Null Hypothesis:  the distribution of two data sets are NOT DIFFERENT
# Alternative Hypothesis:  the distributions of two data sets ARE DIFFFERENT


( output1 <- ks.test(x = ksdata, y = ks2data, alternative = c("two.sided"), exact = TRUE) )



# ----------
output1$p.value


# -->
# p.values > 0.05 indicating that Null Hypothesis is NOT REJECTED



