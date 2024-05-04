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
# Generate data for Continuous vs Ordinal
# ------------------------------------------------------------------------------

y2 <- cut(y, c(-Inf, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, Inf))


str(y2)



# ----------
# x: continuous values
# y2: ordinal values


par(mfrow = c(1,1))

plot(x, y2)

plot(y2, x)



# ------------------------------------------------------------------------------
# Compute Polyserial Correlation
# ------------------------------------------------------------------------------


library(polycor)


polycor::polyserial(x, y2)



# ----------
# ML = TRUE:  estimate by maximum likelihood estimation
# std.err = TRUE:  output the standard error of threshold to check normality of latent variable

polycor::polyserial(x, y2, ML = TRUE, std.err = TRUE)


