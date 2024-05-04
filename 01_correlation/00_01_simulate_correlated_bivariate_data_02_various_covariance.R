setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\02_correlation")


packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Simulate correlated bivariate data:  various covariance matrix
# ------------------------------------------------------------------------------

# sample size
n1 <- 100


# means of the variable
mu1 <- c(0, 0)



# covariance matrix of the variable

( r <- seq(0, 0.9, by = 0.1) )


graphics.off()

par(mfrow = c(3,4))


set.seed(122345)


for(i in 1:length(r)){
  
  Sigma1 <- matrix(c(1, r[i], r[i], 1), ncol = 2)
  
  dat <- mvrnorm(n = n1, mu = mu1, Sigma = Sigma1)
  
  plot(dat, xlab = "x", ylab = "y", main = paste0("r = ", round(cor(dat[,1], dat[,2]), 2)), cex.main = 2, cex = 1.2, pch = 20)
  abline(h = 0, lty = 2, col = "gray")
  abline(v = 0, lty = 2, col = "gray")
  lines(smooth.spline(dat[,1], dat[,2]), col = "blue", lty = 2, lwd = 1)
}



