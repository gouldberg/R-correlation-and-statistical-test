rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

data(aptitude, package = "cocor")


str(aptitude)



# ------------------------------------------------------------------------------
# Pearson Product of Moment Correlation
# ------------------------------------------------------------------------------

x1 <- aptitude$sample1$intelligence.a

y1 <- aptitude$sample1$logic


x2 <- aptitude$sample2$intelligence.a

y2 <- aptitude$sample2$logic



# ----------
par(mfrow = c(1,2))

plot(x1, y1)

plot(x2, y2)



# ----------
cor(x = x1, y = y1, method = "pearson")

cor(x = x2, y = y2, method = "pearson")




# ------------------------------------------------------------------------------
# Test difference of two correlations
# ------------------------------------------------------------------------------

# Null Hypothesis:  True difference in correlations (Pearson) IS ZERO
# Alternative Hypothesis:  True difference in correlation (Pearson) NOT ZERO

library(cocor)


( output <- cocor(~ logic + intelligence.a | logic + intelligence.a, aptitude, alternative = "two.sided", return.htest = TRUE) )



# ----------
# p.value = 0.1125, indicating Null Hypothesis is NOT REJECTED

output$fisher1925$p.value



# ------------------------------------------------------------------------------
# Compare two correlations based on two dependent groups
# ------------------------------------------------------------------------------

# Null Hypothesis:  True difference in correlations (Pearson) IS ZERO
# Alternative Hypothesis:  True difference in correlation (Pearson) NOT ZERO

library(cocor)


( output2 <- cocor(~ knowledge + intelligence.a | logic + intelligence.a, aptitude$sample1, alternative = "two.sided", return.htest = TRUE) )



# ----------
# p.value = 0.052, indicating Null Hypothesis is REJECTED

output2$pearson1898$p.value










