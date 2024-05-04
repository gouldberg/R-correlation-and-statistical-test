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

# calculate the difference by id

library(tidyverse)

sleep_2 <- sleep %>% spread(., key = group, value = extra) %>% data.frame() %>% mutate(dif = X2 - X1)

head(sleep_2)




# ----------
# distribution of differences

library(lattice)

histogram(~ dif, data = sleep_2, breaks = seq(0, 5, by = 0.25))



# ----------
boxplot(sleep_2$dif)



# ----------
# calculate mean of difference

mean(sleep_2$dif)




# ------------------------------------------------------------------------------
# 2-samples t-test (paired)
# ------------------------------------------------------------------------------

# Null Hypothesis:  mean of paired differences IS zero
# Althernative Hypothesis:  mean of paired differences is DIFFERENT FROM zero

# var.equal = FALSE
( output <- t.test(extra ~ group, paired = TRUE, data = sleep, alternative = c("two.sided")) )



# ----------
# p.value is 0.0028 indicates that Null Hypothesis IS REJECTED

output$p.value



# -->
# We can say that there are some paired difference




