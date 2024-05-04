rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# generate sample data
# ------------------------------------------------------------------------------

library(mvtnorm)


set.seed(12345)

data <- rmvnorm(1000, c(0,0), matrix(c(1, 0.5, 0.5, 1), 2, 2))



x <- data[,1]

y <- data[,2]



# ----------
par(mfrow = c(1,1))

plot(x, y)
lines(smooth.spline(x, y), col = "blue", lty = 1)



# sample correlation y Pearson
cor(x, y, method = "pearson")




# ------------------------------------------------------------------------------
# Generate data for Ordinal vs Ordinal
# ------------------------------------------------------------------------------


x2 <- cut(x, c(-Inf, -1.5, -1, -0.5, 0.5, 1, 1.5, Inf))

y2 <- cut(y, c(-Inf, -1.5, 0.5, 1.5, Inf))


str(x2)

str(y2)



# ----------
# x2: ordinal values
# y2: ordinal values


par(mfrow = c(1,1))

plot(x2, y2)



# ------------------------------------------------------------------------------
# Compute Polychoric Correlation
# ------------------------------------------------------------------------------


library(polycor)


polychor(x2, y2)



# ----------
# ML = TRUE:  estimate by maximum likelihood estimation
# std.err = TRUE:  output the standard error of threshold to check normality of latent variable

polychor(x2, y2, ML = TRUE, std.err = TRUE)


