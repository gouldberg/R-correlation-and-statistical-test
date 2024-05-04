rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

vx <- c(301, 311, 325, 291, 388, 402, 325, 361, 287, 261, 238, 361,
        197, 180, 178, 260, 247, 199, 179, 134, 163, 200,
        209, 331, 192, 155, 234, 290, 175, 116, 285, 216, 237, 301,
        343, 247, 316, 395, 324, 138, 245, 228, 214, 374, 235)


fx <- factor(rep(c("A", "B", "C", "D"), c(12, 10, 12, 11)))


( df <- data.frame(val = vx, group = fx) )


head(df)




# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

boxplot(val ~ group, data = df)



# ----------
# Calculate variance by group
aggregate(df$val, by = list(fx), FUN = var)




# ------------------------------------------------------------------------------
# Bartlett Test for more than 3 groups
# ------------------------------------------------------------------------------


# Null Hypothesis:  variance of each group is NOT DIFFERENT
# Alternative Hypothesis:  variance of each group IS DIFFERENT


( output <- bartlett.test(vx ~ fx) )



# ----------
# p.value = 0.15 indicating that Null Hypothesis is NOT REJECTED
output$p.value



