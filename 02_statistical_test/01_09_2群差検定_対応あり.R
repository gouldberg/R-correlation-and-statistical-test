rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

vx <- c(1.83, 1.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)

# vy <- c(0.88, 0.65, 0.60, 1.05, 1.06, 1.29, 1.06, 3.14, 1.29)

vy <- c(0.88, 0.65, 0.60, 1.05, 1.06, 1.29, 1.06, 2.14, 1.29)


df <- data.frame(id = seq(1, 9, by = 1), vx = vx, vy = vy)

df$dif <- vy - vx

head(df)



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

boxplot(df$dif)



# ------------------------------------------------------------------------------
# Sign Test for 2-sample difference
# ------------------------------------------------------------------------------

# Null Hypothesis:  There are NO positive or negative differences between 2 groups
# Alternative Hypothesis:  There are SOME positive or negative differences between 2 groups


# The number of positive or negative differences
( x <- min(sum(df$dif < 0), sum(df$dif > 0)) )


# The number of all differences excluding zero difference
( n <- sum(df$dif != 0) )



# ----------
# Sign Test
( output <- binom.test(x = x, n = n, alternative = c("two.sided")) )



# ----------
# p.value < 0.05 indicating that Null Hypothesis is REJECTED
output$p.value
