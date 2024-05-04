setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\02_correlation")


packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Simulate correlated bivariate data
# ------------------------------------------------------------------------------

library(MASS)


# sample size
n <- 100


# means of the variable
mu <- c(0, 0)



# ----------
( Sigma <- matrix(c(1, 0.4, 0.4, 1), ncol = 2) )


dat <- mvrnorm(n = n, mu = mu, Sigma = Sigma)


head(dat)



# ----------
graphics.off()

par(mfrow = c(1,1))

plot(dat, xlab = "x", ylab = "y", cex.main = 2, cex = 1.2, pch = 20)
abline(h = 0, lty = 2, col = "gray")
abline(v = 0, lty = 2, col = "gray")




# ----------
cov(dat)


( vx <- sum((dat[,1] - mean(dat[,1]))^2) / ( n - 1 ) )
var(dat[,1])

( vy <- sum((dat[,2] - mean(dat[,2]))^2) / ( n - 1 ) )
var(dat[,2])


( sxy <- sum((dat[,1] - mean(dat[,1])) * (dat[,2] - mean(dat[,2]))) / ( n - 1 ) )



# ----------
cor(dat[,1], dat[,2])


sxy / ( sqrt(vx) * sqrt(vy) )


