rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# 1-sample test for proportion by binom.test
#  - perform an exact test of a simple null hypothesis about the probability of success in a Bernoulli experiment
# ------------------------------------------------------------------------------

# Null Hypothesis:  Proportion of success is NOT DIFFERENT from 0.40 (= 8 / 20)
# Alternative Hypothesis:  Proportion of success IS DIFFERENT from 0.40 (= 8 / 20)


( output1 <- binom.test(x = 17, n = 25, p = 8 / 20, alternative = c("two.sided")) )




# ----------
# p.value < 0.05 indicating that Null Hypothesis IS REJECTED

output1$p.value

