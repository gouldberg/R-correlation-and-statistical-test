rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

pref <- c(7, 8, 9, 5, 6, 5, 4, 7, 1, 3, 8, 6, 7, 2, 5)


subject <- factor(c(rep("線形代数", 5), rep("微分積分", 5), rep("確率統計", 5)))

person <- factor(rep(c("田中", "岸", "大丸", "吉川", "荻野"), 3))



( df <- data.frame(person = person, subject = subject, pref = pref) )


library(tidyverse)

( tab <- spread(df, key = subject, value = pref) )



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

matplot(as.matrix(tab), type = "l")


library(lattice)

boxplot(pref ~ subject, data = df)




# ------------------------------------------------------------------------------
# Paired One-Way Anova (Analysis of Variance) by aov
# ------------------------------------------------------------------------------

# Null Hypothesis:  mean of preference of subjects are NOT DIFFERENT among all persons
# Althernative Hypothesis:  mean of preference of subjects ARE DIFFERENT among all persons

( output1 <- aov(pref ~ subject, data = df) )



# ----------
# p.value > 0.05, indicating  that Null Hypothesis IS NOT REJECTED
summary(output1)


