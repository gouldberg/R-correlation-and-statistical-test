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


( df <- data.frame(Test = Test, LearningMethod = LearningMethod) )




# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

library(lattice)

xyplot(Test ~ LearningMethod, data = df)



# ----------
boxplot(Test ~ LearningMethod, data = df)




# ------------------------------------------------------------------------------
# Multiple Comparison
# ------------------------------------------------------------------------------

# Null Hypothesis:  mean of paired differences IS zero
# Althernative Hypothesis:  mean of paired difference is DIFFERENT from zero

( output1 <- TukeyHSD(aov(Test~ LearningMethod, data = df)) )


plot(output1)



# -->
# There is difference in mean in C-A and D-A

