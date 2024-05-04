rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

sleep


str(sleep)



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

# distribution by group

library(lattice)

histogram(~ extra | group, data = sleep)



# ----------
# we can see that both distributions are quite different from each other ...

par(mfrow = c(1,1))
boxplot(extra ~ group, data = sleep)



# ----------
# calculate mean by group

with(sleep, by(extra, group, mean))



# ------------------------------------------------------------------------------
# Test of normality for group 1
# ------------------------------------------------------------------------------


# Shapiro - Wilk Test
# Null Hypothesis:  objective data DOES follow normal distribution
# Alternative Hypothesis:  objective data DOES NOT follow normal distribution


( output1 <- shapiro.test(sleep$extra[sleep$group == 1]) )




# ----------
# Kolmogrov - Smirnov Normality Test
# Null Hypothesis:  objective data DOES follow normal distribution with specified mu and sd
# Alternative Hypothesis:  objective data DOES NOT follow normal distribution with specified mu and sd


( mu <- mean(sleep$extra[sleep$group == 1]) )

( sd <- sd(sleep$extra[sleep$group == 1]) )

( output2 <- ks.test(x = sleep$extra[sleep$group == 1], y = "pnorm", mean = mu, sd = sd, alternative = c("two.sided"), exact = TRUE) )




# ----------
# p.value is 0.4079 and 0.8051 indicates that Null Hypothesis is NOT REJECTED by BOTH tests


output1$p.value

output2$p.value



# ------------------------------------------------------------------------------
# Test of normality for group 2
# ------------------------------------------------------------------------------

# ----------
# Shapiro - Wilk Test
( output1 <- shapiro.test(sleep$extra[sleep$group == 2]) )




# ----------
# Kolmogrov - Smirnov Normality Test

( mu <- mean(sleep$extra[sleep$group == 2]) )

( sd <- sd(sleep$extra[sleep$group == 2]) )

( output2 <- ks.test(x = sleep$extra[sleep$group == 2], y = "pnorm", mean = mu, sd = sd, alternative = c("two.sided"), exact = TRUE) )




# ----------
# Both tests does NOT REJECTED

output1$p.value

output2$p.value



# ----------
# check qqnorm against qqline

graphics.off()

par(mfrow = c(1,2))

qqnorm(sleep$extra[sleep$group == 1])

qqline(sleep$extra[sleep$group == 1])

qqnorm(sleep$extra[sleep$group == 2])

qqline(sleep$extra[sleep$group == 2])



# ------------------------------------------------------------------------------
# One-sample t-test for group 1
# ------------------------------------------------------------------------------


# Null Hypothesis:  mean of "extra" of group 1 is NOT DIFFERENT from 1
# Alternative Hypothesis:  mean of "extra" of group 1 is DIFFERENT from 1

( output <- t.test(sleep$extra[sleep$group == 1], mu = 1, alternative = c("two.sided")) )



# ----------
# p.value is 0.67, indicating that Null Hypothesis is NOT REJECTED
output$p.value




# ----------
# We can NOT say that mean of "extra" of group 1 is DIFFERENT from 1

