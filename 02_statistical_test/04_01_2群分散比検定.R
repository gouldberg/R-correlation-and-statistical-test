rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

va <- c(301, 311, 325, 291, 388, 412, 325, 361, 287)


vb <- c(197, 180, 247, 260, 247, 199, 179, 134, 163, 200)


( df <- data.frame(val = c(va, vb), group = c(rep(1, 9), rep(2, 10))) )




# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

boxplot(val ~ group, data = df)


var(va)

var(vb)



# ------------------------------------------------------------------------------
# F test for 2-sample variance from normal populations
# ------------------------------------------------------------------------------


# Null Hypothesis:  variance from 2 groups are NOT DIFFERENT
# Alternative Hypothesis:  variance from 2 groups ARE DIFFERENT


( output <- var.test(va, vb, alternative = c("two.sided")) )



# ----------
# p.value = 0.7859 indicating that Null Hypothesis is NOT REJECTED
output$p.value



