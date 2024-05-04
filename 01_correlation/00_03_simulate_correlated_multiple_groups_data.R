setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\02_correlation")


packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Simulate group mean data
# ------------------------------------------------------------------------------

library(MASS)



# ----------
# mean of 10 groups

par(mfrow = c(1,1))

dat_mean <- mvrnorm(n = 10, mu = c(0, 0), Sigma = matrix(c(1, 0.7, 0.7, 1), ncol = 2))

dat_mean <- data.frame(x = dat_mean[,1], y = dat_mean[,2], group = 1:10)



plot(y ~ x, data = dat_mean, pch = 20, cex = 3, col = "black", 
     main = paste0("10 groups mean, correlation is ", round(cor(dat_mean$x, dat_mean$y), 3)), 
     xlim = c(-4, 4), ylim = c(-4, 4), cex.main = 2)

abline(lm(y ~ x, data = dat_mean), lty = 2, col = "black")




# ------------------------------------------------------------------------------
# Simulate records by each group
# ------------------------------------------------------------------------------

# sample size
n <- seq(30, 130, by = 10)


# covariance matrix of the variable
r <- c(-0.4, -0.3, -0.2, 0.3, 0.4, 0.8, 0.4, 0.3, 0.2, 0.4)




# ----------
# by organization

graphics.off()

par(mfrow = c(3,4))


set.seed(122345)


dat_all <- data.frame()


for(i in 1:length(r)){
  
  Sigma1 <- matrix(c(1, r[i], r[i], 1), ncol = 2)
  
  dat <- mvrnorm(n = n[i], mu = c(dat_mean[i,1], dat_mean[i,2]), Sigma = Sigma1)
  
  tmp <- data.frame(x = dat[,1], y = dat[,2], group = i)
  
  dat_all <- rbind(dat_all, tmp)

  plot(dat, main = paste0("r = ", round(cor(dat[,1], dat[,2]), 2)), cex.main = 2, cex = 1.2, pch = 20)
  abline(h = dat_mean[i,2], lty = 2, col = "black")
  abline(v = dat_mean[i,1], lty = 2, col = "black")
}




# ------------------------------------------------------------------------------
# plot all in one
# ------------------------------------------------------------------------------

graphics.off()

plot(y ~ x, data = dat_all, pch = 1:10, col = 1:10, 
     main = paste0("Pearson Correlation: ", round(cor(dat_all$x, dat_all$y), 3)), cex = 1.2)

points(y ~ x, data = dat_mean, pch = 20, cex = 3, col = "black")



# ----------
abline(lm(y ~ x, data = dat_all), lwd = 4, col = "black")

for(i in 1:10){ abline(lm(y ~ x, data = dat_all[dat_all$group == i,]), col = i) }




# ------------------------------------------------------------------------------
# cut-off some points partially
# ------------------------------------------------------------------------------

library(dplyr)


thre_y <- -2.0

thre_x <- -2.5


dat_all_cut <- dat_all %>% filter(x > thre_x, y > thre_y)

summary(dat_all_cut)


# dat_all_cut <- dat_all %>% filter(y > thre_y)



# ----------
graphics.off()

plot(y ~ x, data = dat_all_cut, pch = 1:10, col = 1:10, 
     main = paste0("Pearson Correlation: ", round(cor(dat_all_cut$x, dat_all_cut$y), 3)), 
     cex = 1.2, ylim = c(-4, 4), xlim = c(-4, 4), cex.main = 2)

points(y ~ x, data = dat_mean, pch = 20, cex = 3, col = "black")



round(cor(dat_all$x, dat_all$y), 3)

round(cor(dat_all_cut$x, dat_all_cut$y), 3)


