rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

vx <- c(1.83, 1.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)

vy <- c(0.88, 0.65, 0.60, 1.05, 1.06, 1.29, 1.06, 2.14, 1.29)



df <- data.frame(val = c(vx, vy), group = c(rep("x", 9), rep("y", 9)))

head(df)


df2 <- data.frame(id = seq(1, 9, by = 1), vx = vx, vy = vy)


head(df2)



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

matplot(as.matrix(df2[,c("vx", "vy")]), type = "l")

boxplot(val ~ group, data = df)



# ----------
# calculate by group median, mean, and variance

aggregate(df$val, by = list(df$group), FUN = "median")

aggregate(df$val, by = list(df$group), FUN = "mean")

aggregate(df$val, by = list(df$group), FUN = "var")




# ------------------------------------------------------------------------------
# Shapiro Wilk normality test: shapiro.test
# ------------------------------------------------------------------------------


# Null Hypothesis:  objective data DOES follow normal distribution
# Alternative Hypothesis:  objective data DOES NOT follow normal distribution


( output1 <- shapiro.test(vx) )

( output2 <- shapiro.test(vy) )



# ----------
# Both of p.value > 0.05, indivating that Null Hypothesis are NOT REJECTED

output1$p.value

output2$p.value



# ----------
# Check Q-Q normal plot
graphics.off()
par(mfrow = c(1, 2))

qqnorm(vx, main = "Normal Q-Q plot for vx")
qqline(vx)


qqnorm(vy, main = "Normal Q-Q plot for vy")
qqline(vy)



# ----------
x <- ecdf(vx)

y <- ecdf(vy)


graphics.off()
par(mfrow = c(1, 2))

plot(x)
plot(y)



# ------------------------------------------------------------------------------
# Wilcoxon rank sum test for 2-samples true location (paired)
# ------------------------------------------------------------------------------

# Null Hypothesis:  central location of 2 gorups are NOT DIFFERENT
# Alternative Hypothesis:  central locaton of 2 groups are DIFFERENT


library(exactRankTests)

output1 <- wilcox.exact(x = vx, y = vy, paired = T, alternative = c("two.sided"))



# ----------
# p.value < 0.05 indicating that Null Hypothesis IS REJECTED

output1$p.value



# ------------------------------------------------------------------------------
# For reference:  2-samples t-test with var.equal = FALSE (no paired)
# ------------------------------------------------------------------------------


( output0 <- t.test(Ozone ~ Month, var.equal = FALSE, data = airquality, alternative = c("two.sided"), subset = Month %in% c(5, 8)) )


output0$p.value



