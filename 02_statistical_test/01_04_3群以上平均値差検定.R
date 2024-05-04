rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

A <- c(15, 9, 18, 14, 18)

B <- c(13, 8, 8, 12, 7)

C <- c(10, 6, 11, 7, 12)

D <- c(10, 7, 3, 5, 7)


( Test <- c(A, B, C, D) )



# ----------
LearningMethod <- c(rep("A", 5), rep("B", 5), rep("C", 5), rep("D", 5))


( LearningMethod <- factor(LearningMethod) )


( df <- data.frame(Test =Test, LearningMethod = LearningMethod) )




# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

library(lattice)

xyplot(Test ~ LearningMethod, data = df, cex = 2, pch = 20)



# ----------
boxplot(Test ~ LearningMethod, data = df)





# ------------------------------------------------------------------------------
# One-Way Anova (Analysis of Variance) by oneway.test and aov
# ------------------------------------------------------------------------------

# Null Hypothesis:  mean of all groups are NOT DIFFERENT
# Althernative Hypothesis:  mean of all groups ARE DIFFERENT (at least 1 group has different mean)

( output1 <- oneway.test(Test ~ LearningMethod, var.equal = FALSE, data = df) )



# ----------
# aov gives result of the table of analysis of variance
( output2 <- aov(Test ~ LearningMethod, data = df) )



# ----------
# p.value is 0.27 indicating that Null Hypothesis is REJECTED
output1$p.value


