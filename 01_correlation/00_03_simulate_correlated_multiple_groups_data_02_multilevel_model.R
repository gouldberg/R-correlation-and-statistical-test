setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\02_correlation")


packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Simulate group mean data
# ------------------------------------------------------------------------------

library(MASS)



# ----------
# mean of 10 groups

dat_mean <- mvrnorm(n = 10, mu = c(0, 0), Sigma = matrix(c(1, 0.7, 0.7, 1), ncol = 2))

dat_mean <- data.frame(x = dat_mean[,1], y = dat_mean[,2], group = 1:10)



plot(y ~ x, groups = as.factor(group), data = dat_mean, pch = 20, cex = 3, col = "black", 
     main = paste0("10 groups mean, correlation is ", round(cor(dat_mean$x, dat_mean$y), 3)), 
     xlim = c(-4, 4), ylim = c(-4, 4), cex.main = 2)

abline(lm(y ~ x, data = dat_mean), lty = 2, col = "black")




# ------------------------------------------------------------------------------
# Simulate records by each group
# ------------------------------------------------------------------------------

# sample size
n <- seq(30, 130, by = 10)


# covariance matrix of the variable
# r <- c(-0.4, -0.3, -0.2, 0.3, 0.4, 0.8, 0.4, 0.3, 0.2, 0.4)

r <- rnorm(n = 10, mean = 0.5, sd = 0.2)



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

plot(y ~ x, groups = as.factor(group), data = dat_all, pch = 1:10, col = 1:10, 
                main = paste0("Pearson Correlation: ", round(cor(dat_all$x, dat_all$y), 3)), cex = 1.2)

points(y ~ x, data = dat_mean, pch = 20, cex = 3, col = "black")



# ----------
abline(lm(y ~ x, data = dat_all), lwd = 4, col = "black")

for(i in 1:10){ abline(lm(y ~ x, data = dat_all[dat_all$group == i,]), col = i) }



# ------------------------------------------------------------------------------
# bivariate pairs plot
# ------------------------------------------------------------------------------

library(car)

formula <- ~ y + x


scatterplotMatrix(formula, data = dat_all,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)


# ----------
# by group

formula <- ~ y + x | as.factor(group)

scatterplotMatrix(formula, data = dat_all,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = 1:10, pch = 1:10)



# ------------------------------------------------------------------------------
# Test difference by group
# ------------------------------------------------------------------------------

by(dat_all$y, dat_all$group, FUN = mean)


aggregate(dat_all$x, by = list(dat_all$group), FUN = "var")



# ----------
# Note that one-way anova does not apply due to unequal sample size
aov(y ~ group, data = dat_all)




# ----------
# Kruskal-Wallis rank sum test
kruskal.test(y ~ group, data = dat_all)


# -->
# rejected the null hypothesis that the medians are not different




# ------------------------------------------------------------------------------
# linear regression
# ------------------------------------------------------------------------------

glin <- lm(y ~ x, data = dat_all)

summary(glin)



# ----------
# Analysis of Variance Table by each term
anova(glin)




# ----------
plot(y ~ x, data = dat_all)
abline(glin)




# ----------
glin2 <- lm(y ~ x + as.factor(group), data = dat_all)

summary(glin2)




# ----------
anova(glin2)


AIC(glin, glin2)




# ------------------------------------------------------------------------------
# The Random Effects model
# ------------------------------------------------------------------------------


lmemod1 <- lme(y ~ 1, random = ~ 1 | as.factor(group), data = dat_all)


summary(lmemod1)



# ----------
F0 <- fitted(lmemod1, level = 0)

F1 <- fitted(lmemod1, level = 1)


I <- order(dat_all$x)

xs <- sort(dat_all$x)

plot(y ~ x, group = as.factor(dat_all$group), data = dat_all, pch = 1:10, col = 1:10)
lines(xs, F0[I], lwd = 4, type = "l")

for(i in 1:10){
  x1 <- dat_all$x[dat_all$group == i]
  y1 <- F1[dat_all$group == i]
  K <- order(x1)
  lines(sort(x1), y1[K])
}



# ------------------------------------------------------------------------------
# The Random Inercept Model
# ------------------------------------------------------------------------------


library(nlme)


lmemod2 <- lme(y ~ x, random = ~ 1 | as.factor(group), data = dat_all)


summary(lmemod2)

summary(glin2)



# -->
# residual variance:  0.84 ^ 2, different from glin2 (1.025 ^ 2)
# variance for random intercept:  0.76 ^ 2

# fixed effects coefficients = 0.420, different from glin2 (0.448)



# ----------
F0 <- fitted(lmemod2, level = 0)

F1 <- fitted(lmemod2, level = 1)


I <- order(dat_all$x)

xs <- sort(dat_all$x)

plot(y ~ x, group = as.factor(dat_all$group), data = dat_all, pch = 1:10, col = 1:10)
lines(xs, F0[I], lwd = 4, type = "l")

for(i in 1:10){
  x1 <- dat_all$x[dat_all$group == i]
  y1 <- F1[dat_all$group == i]
  K <- order(x1)
  lines(sort(x1), y1[K])
}




# ------------------------------------------------------------------------------
# The Random Inercept and Slope model
# ------------------------------------------------------------------------------


lmemod3 <- lme(y ~ x, random = ~ 1 + x | as.factor(group), data = dat_all)


summary(lmemod3)



# -->
# residual variance:  0.83 ^ 2 (a little bit smaller)
# variance for random intercept:  0.71 ^ 2

# fixed effects coefficients = 0.420


# ----------
F0 <- fitted(lmemod3, level = 0)

F1 <- fitted(lmemod3, level = 1)


I <- order(dat_all$x)

xs <- sort(dat_all$x)

plot(y ~ x, group = as.factor(dat_all$group), data = dat_all, pch = 1:10, col = 1:10)
lines(xs, F0[I], lwd = 4, type = "l")

for(i in 1:10){
  x1 <- dat_all$x[dat_all$group == i]
  y1 <- F1[dat_all$group == i]
  K <- order(x1)
  lines(sort(x1), y1[K])
}




# ------------------------------------------------------------------------------
# Less general random effects structure:  b(j) ~ N(0, phi), phi is a diagonal matrix (with positive diagonal elements)
# ------------------------------------------------------------------------------


lmemod4 <- lme(y ~ x, data = dat_all, 
          random = list(group = pdDiag(~x)))


summary(lmemod4)



# ----------
F0 <- fitted(lmemod4, level = 0)

F1 <- fitted(lmemod4, level = 1)


I <- order(dat_all$x)

xs <- sort(dat_all$x)

plot(y ~ x, group = as.factor(dat_all$group), data = dat_all, pch = 1:10, col = 1:10)
lines(xs, F0[I], lwd = 4, type = "l")

for(i in 1:10){
  x1 <- dat_all$x[dat_all$group == i]
  y1 <- F1[dat_all$group == i]
  K <- order(x1)
  lines(sort(x1), y1[K])
}





# ------------------------------------------------------------------------------
# Model comparison:  AIC and BIC by "REML"
# ------------------------------------------------------------------------------


lmemod1 <- lme(y ~ 1, random = ~ 1 | as.factor(group), data = dat_all, method = "REML")


lmemod2 <- lme(y ~ x, random = ~ 1 | as.factor(group), data = dat_all, method = "REML")


lmemod3 <- lme(y ~ x, random = ~ 1 + x | as.factor(group), data = dat_all, method = "REML")


lmemod4 <- lme(y ~ x, data = dat_all, random = list(group = pdDiag(~x)))


BIC(lmemod1, lmemod2, lmemod3, lmemod4)




# ------------------------------------------------------------------------------
# Model comparison:  likelihood ratio test using ML estimation
# ------------------------------------------------------------------------------


lmemod1 <- lme(y ~ 1, random = ~ 1 | as.factor(group), data = dat_all, method = "ML")


lmemod2 <- lme(y ~ x, random = ~ 1 | as.factor(group), data = dat_all, method = "ML")


lmemod3 <- lme(y ~ x, random = ~ 1 + x | as.factor(group), data = dat_all, method = "ML")


lmemod4 <- lme(y ~ x, data = dat_all, random = list(group = pdDiag(~x)))



anova(lmemod1, lmemod2)


anova(lmemod2, lmemod3)


anova(lmemod3, lmemod4)




# ------------------------------------------------------------------------------
# Standardized residuals against fitted value and x, with by group
# ------------------------------------------------------------------------------

mod_obj <- lmemod3


plot(mod_obj)


plot(mod_obj, resid(., type = "p") ~ fitted(.) | group, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)

plot(mod_obj, resid(., type = "p") ~ x | group, abline = 0, lty = 2, col = gray(0.4), cex = 0.8, pch = 20)




# ----------
plot(mod_obj, group ~ resid(., type = "p"))




# ------------------------------------------------------------------------------
# Normal QQ plots for the predicted random effects
# ------------------------------------------------------------------------------


mod_obj <- lmemod3

qqnorm(mod_obj, ~ ranef(.))



# -->
# The plots should look like correlated random scatters around straight lines,
# if the normality assumptions for the random effects are reasonable.



# ------------------------------------------------------------------------------
# The collection of index plot
#   - for mixed-effects models, influence() refits the model deleting each of group in turn,
#     and provide deletion statistics
#   - influenceIndexPlot() create plots for both the fixed effects, displaying influence on the coefficients and Cook's distances,
#     and the random-effect parameters
# ------------------------------------------------------------------------------

mod_obj <- lmemod3


infl <- influence(mod_obj)


infl



# ------------------------------------------------------------------------------
# Model understanding:  x effect
# ------------------------------------------------------------------------------

mod_obj <- lmemod3

( rf1 <- ranef(mod_obj)$x )


summary(rf1)



# -->
# for x' effect, the difference between the best and the worst is about 0.56



# ------------------------------------------------------------------------------
# Model understanding:  Fixed Effects
# ------------------------------------------------------------------------------

mod_obj <- lmemod3


library(effects)

plot(predictorEffects(mod_obj))



# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve
plot(predictorEffects(mod_obj, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))

# eff2 <- effects::allEffects(mod_obj, partial.residuals = TRUE)



# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

fitp <- cbind(dat_all, y_pred = predict(mod_obj, type = "response"))

# fitp <- fortify(mod_obj)

head(fitp)




library(ggplot2)


graphics.off()

gg <- ggplot(fitp, aes(x = x, y = y_pred)) + theme_bw() + geom_point(colour = "black", size = 1.5)

gg + facet_wrap(~ group) + stat_smooth()




# ------------------------------------------------------------------------------
# Predicttion
# ------------------------------------------------------------------------------

# predicted coefficients
fixef(mod_obj)

ranef(mod_obj)



# ----------
# We specify the random effects part of the prediction as ~0 meaning that this term is not present.
predict(mod_obj, re.form = ~ 0)

predict(mod_obj)



# ----------
predict(mod_obj, newdata = data.frame(group = 10, x = 10))




# ------------------------------------------------------------------------------
# Plot predicted values
# ------------------------------------------------------------------------------

# augPred():  predicted values are obtained at the specified values of primary.
# If object has a grouping structure, predicted values are obtained for each group.

# This is model predictions from m2 at the individual tree level, overlaid on individual Loblolly pine growth data.
# The panel titles are the value of the Seed tree identifier.

augPred(mod_obj)

plot(augPred(mod_obj))

