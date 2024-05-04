rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

x <- c(70, 72, 62, 64, 71, 76, 60, 65, 74, 72)

y <- c(70, 74, 65, 68, 72, 74, 61, 66, 76, 75)


y2 <- order(y, decreasing = TRUE)



# ----------
par(mfrow = c(1,2))

plot(x, y)

plot(x, y2)




# ------------------------------------------------------------------------------
# Correlation by Pearson's Product of Moment
# ------------------------------------------------------------------------------

cor(x, y, method = c("pearson"))

cor(x, y2, method = c("pearson"))



# ------------------------------------------------------------------------------
# Test No Correlation
# ------------------------------------------------------------------------------

# Null Hypothesis:  There is NO CORRELATION (Pearson) between 2 samples
# Alternative Hypothesis:  There is SOME CORRELATION (Pearson) between 2 samples


output1 <- cor.test(x, y, method = c("pearson"))

output2 <- cor.test(x, y2, method = c("pearson"))



# ----------
# p.value < 0.05, almost zero, indicating Null Hypothesis IS REJECTED

output1$p.value


# p.value < 0.97, indicating Null Hypothesis IS NOT REJECTED

output2$p.value


