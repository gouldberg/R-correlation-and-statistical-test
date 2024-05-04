rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

vx <- c(3.7, 3.1, 2.5, 2.6, 3.2, 3.2, 2.5)

vy <- c(3.8, 2.7, 4.0, 2.4, 2.2, 3.4, 5.5)

vz <- c(2.8, 3.4, 4.5, 2.2, 3.0, 3.1, 3.4)



df <- data.frame(id = seq(1, 7, by = 1), vx = vx, vy = vy, vz = vz)

head(df)



df2 <- data.frame(val = c(vx, vy, vz), group = c(rep("x", 7), rep("y", 7), rep("z", 7)))

head(df2)




# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))


matplot(as.matrix(df[,c("vx", "vy", "vz")]), type = "l")

boxplot(val ~ group, data = df2)



# ----------
# calculate by group: median, mean, variance
aggregate(df2$val, by = list(df2$group), FUN = "median")

aggregate(df2$val, by = list(df2$group), FUN = "mean")

aggregate(df2$val, by = list(df2$group), FUN = "var")




# ------------------------------------------------------------------------------
# Freedman Test
# ------------------------------------------------------------------------------

# Null Hypothesis:  the medians of each variable (paired) are NOT DIFFERENT
# Alternative Hypothesis:  the medians of each variable (paired) ARE DIFFERENT


( output <- friedman.test(y = matrix(c(vx, vy, vz), ncol = 3)) )



# ----------
# p.value > 0.05 indicating that Null Hypothesis is NOT REJECTED
output$p.value





# ------------------------------------------------------------------------------
# Other data
# ------------------------------------------------------------------------------

pref <- c(7, 8, 9, 5, 6, 5, 4, 7, 1, 3, 8, 6, 7, 2, 5)


subject <- factor(c(rep("線形代数", 5), rep("微分積分", 5), rep("確率統計", 5)))

person <- factor(rep(c("田中", "岸", "大丸", "吉川", "荻野"), 3))


( df3 <- data.frame(person = person, subject = subject, pref = pref) )


library(tidyverse)

( tab <- spread(df3, key = subject, value = pref) )



# ----------
matplot(as.matrix(tab), type = "l")


library(lattice)

boxplot(pref ~ subject, data = df)




# ----------
# One-Way ANOVA for paired data:
# Null Hypothesis:  the mean of preference of subjects are NOT DIFFERENT among all persons
# Alternative Hypothesis:  the mean of preference of subjects ARE DIFFERENT among all persons


( output <- aov(pref ~ subject + person, data = df3) )


# p.value for subject < 0.05, indicating that Null Hypothesis IS REJECTED.
summary(output)



# ----------
# Friedman Test for paired data:
# Null Hypothesis:  the mean of each variable (paired) are NOT DIFFERENT
# Alternative Hypothesis:  the mean of each variable (paired) ARE DIFFERENT


( output2 <- friedman.test(y = matrix(c(6,2,7,8,5,8,5,9,7,6,4,1,7,5,3), ncol = 3)) )


# p.value for subject < 0.05, indicating that Null Hypothesis IS REJECTED.
output2$p.value
