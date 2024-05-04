rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

vx <- c(2.9, 3.0, 2.5, 2.6, 3.2, 3.2, 2.5, 4.4)

vy <- c(3.8, 2.7, 4.0, 2.4)

vz <- c(1.8, 2.4, 2.5, 2.2, 2.0, 3.1)


df <- data.frame(val = c(vx, vy, vz), group = c(rep("x", 8), rep("y", 4), rep("z", 6)))

head(df)



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

boxplot(val ~ group, data = df)



# ----------
# calculate by group: median, mean, variance
aggregate(df$val, by = list(df$group), FUN = "median")

aggregate(df$val, by = list(df$group), FUN = "mean")

aggregate(df$val, by = list(df$group), FUN = "var")




# ------------------------------------------------------------------------------
# Kruskal-Wallice Test
# ------------------------------------------------------------------------------

# Null Hypothesis:  the medians of each group are NOT DIFFERENT
# Alternative Hypothesis:  the medians of each group ARE DIFFERENT


( output <- kruskal.test(x = list(vx, vy, vz)) )



# ----------
# p.value < 0.05 indicating that Null Hypothesis is REJECTED, some of the medians are different from others.
output$p.value
