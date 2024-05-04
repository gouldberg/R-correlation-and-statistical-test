rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------


vx <- c(58, 99, 32, 48)


p_theory <- c(0.3, 0.5, 0.1, 0.1)


( expected <- p_theory * sum(vx) )



# ------------------------------------------------------------------------------
# Goodness of Fit Test based on Chi-Squared value
# ------------------------------------------------------------------------------

# Null Hypothesis:  data is sampled from specified distribution ("p_theory")
# Alternative Hypothesis:  data is NOT sampled from specified distribution ("p_theory")


output1 <- chisq.test(x = vx, p = p_theory)


output2 <- chisq.test(x = vx, p = expected, rescale.p = TRUE)



# ----------
# p.value is almost zero, indicating that Null Hypothesis IS REJECTED

output1$statistic

output1$p.value



# ----------
# p.value is almost zero, indicating that Null Hypothesis IS REJECTED

output2$statistic

output2$p.value



# ------------------------------------------------------------------------------
# Other data
# ------------------------------------------------------------------------------

# binomial distribution with different probability

( x <- rbinom(n = 10, size = 100, prob = 0.10) )


( y <- rbinom(n = 10, size = 100, prob = 0.12) )



par(mfrow = c(2, 1))

plot(density(x))

plot(density(y))



output3 <- chisq.test(x = x, p = y, rescale.p = TRUE)



# ----------

output3$statistic

output3$p.value
