rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

women


str(women)


# -->
# Note that the distribution of women's height is uniform distribution with seq(58, 72, 1)



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

boxplot(women$height)



# ----------
# calculate mean by group

mean(women$height)



# ------------------------------------------------------------------------------
# Shapiro Wilk normality test:  shapiro.test
# ------------------------------------------------------------------------------

# Null Hypothesis:  objective data DOES follow normal distribution
# Alternative Hypothesis:  objective data DOES NOT follow normal distribution


( output1 <- shapiro.test(women$height) )



# ----------
# p.value = 0.754 indicating that Null Hypothesis is NOT REJECTED
output1$p.value



# ------------------------------------------------------------------------------
# Kolmogorov-Smirnov normality test:  ks.test
# ------------------------------------------------------------------------------

# Null Hypothesis:  objective data DOES follow normal distribution with specified mu and sd
# Alternative Hypothesis:  objective data DOES NOT follow normal distribution with specified mu and sd


( mu <- mean(women$height) )

( sd <- sd(women$height) )

( output2 <- ks.test(x = women$height, y = "pnorm", mean = mu, sd = sd, alternative = c("two.sided"), exact = TRUE) )



# ----------
output1$p.value

output2$p.value



# -->
# p.values > 0.05 indicating that Null Hypothesis is NOT REJECTED by both tests



# ------------------------------------------------------------------------------
# Q-Q plot
# ------------------------------------------------------------------------------

# check qqnorm against qqline

graphics.off()

par(mfrow = c(1,1))

qqnorm(women$height)

qqline(women$height)




# ------------------------------------------------------------------------------
# ECDF (Empirical Cumulative Distribution Function)
# ------------------------------------------------------------------------------


x <- ecdf(women$height)


plot(x)

knots(x)

summary(x)

