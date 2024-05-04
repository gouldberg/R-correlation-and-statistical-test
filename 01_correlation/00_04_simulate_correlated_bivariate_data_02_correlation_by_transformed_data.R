setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\02_correlation")


packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Simulate correlated bivariate data + linear transoformation
# ------------------------------------------------------------------------------

library(MASS)



# sample size
n1 <- 100


# means of the variable
mu1 <- c(0, 0)



# ----------
r <- seq(0, 0.9, by = 0.1)



# ----------
# intercept and coefficient for transformation
a <- rnorm(n = length(r), mean = 1, sd = 2)

b <- rnorm(n = length(r), mean = -1, sd = 1)



graphics.off()

par(mfrow = c(3,4))


set.seed(122345)


for(i in 1:length(r)){
  
  Sigma1 <- matrix(c(1, r[i], r[i], 1), ncol = 2)
  
  dat <- mvrnorm(n = n1, mu = mu1, Sigma = Sigma1)
  

  # ----------
  # transformation:  Y = a + b * Y
  dat[,2] <- a[i] + b[i] * dat[,2]
  
  plot(dat, xlab = 0, ylab = 0, main = paste0("r = ", round(cor(dat[,1], dat[,2]), 2)), cex.main = 2, cex = 1.2, pch = 20)
  lines(smooth.spline(dat[,1], dat[,2]), col = "blue", lty = 2, lwd = 1)
}



# -->
# Note that "absolute" value of Peason's correlations are NOT changed by affine transformation


