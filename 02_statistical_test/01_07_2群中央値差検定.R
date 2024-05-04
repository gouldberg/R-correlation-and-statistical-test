rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

airquality


str(airquality)




# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

boxplot(Ozone ~ Month, data = airquality)



# ----------
# Distribution statistics for May and Sugust

psych::describe(airquality[airquality$Month == 5,])

psych::describe(airquality[airquality$Month == 8,])



# -->
# Note that especially Ozone in May has outlier and the value of skew and kurtosis is large.




# ------------------------------------------------------------------------------
# Shapiro Wilk normality test: shapiro.test
# ------------------------------------------------------------------------------


# Null Hypothesis:  objective data DOES follow normal distribution
# Alternative Hypothesis:  objective data DOES NOT follow normal distribution


( output1 <- shapiro.test(airquality[airquality$Month == 5, "Ozone"]) )

( output2 <- shapiro.test(airquality[airquality$Month == 8, "Ozone"]) )



# ----------
# For May, the Ozone is NOT follow normal distribution

output1$p.value

output2$p.value



# ----------
# Check Q-Q normal plot
graphics.off()
par(mfrow = c(1, 2))

qqnorm(airquality[airquality$Month == 5, "Ozone"], main = "Normal Q-Q plot for May Ozone")
qqline(airquality[airquality$Month == 5, "Ozone"])


qqnorm(airquality[airquality$Month == 8, "Ozone"], main = "Normal Q-Q plot for August Ozone")
qqline(airquality[airquality$Month == 8, "Ozone"])



# ----------
x5 <- ecdf(airquality[airquality$Month == 5, "Ozone"])

x8 <- ecdf(airquality[airquality$Month == 8, "Ozone"])


graphics.off()
par(mfrow = c(1, 2))

plot(x5)
plot(x8)


knots(x5)
summary(x5)

summary(x8)



# ------------------------------------------------------------------------------
# Wilcoxon rank sum test for 2-samples true location (no paired)
# ------------------------------------------------------------------------------


# Null Hypothesis:  central location of 2 gorups are NOT DIFFERENT
# Alternative Hypothesis:  central locaton of 2 groups are DIFFERENT


# output3 <- wilcox.test(Ozone ~ Month, alternative = c("two.sided"), correct = TRUE, data = airquality, subset = Month %>% c(5,8))


library(exactRankTests)

output3 <- wilcox.exact(Ozone ~ Month, alternative = c("two.sided"), data = airquality, subset = Month %>% c(5,8))



# ----------
# p.value < 0.05 indicating that Null Hypothesis IS REJECTED

output3$p.value



# ------------------------------------------------------------------------------
# For reference:  2-samples t-test with var.equal = FALSE (no paired)
# ------------------------------------------------------------------------------


( output0 <- t.test(Ozone ~ Month, var.equal = FALSE, data = airquality, alternative = c("two.sided"), subset = Month %in% c(5, 8)) )


output0$p.value





