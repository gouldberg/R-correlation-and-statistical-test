rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

cmhdemo1 <- read.table(header = TRUE, sep = "", text = "
         b1  b2   b3  b4  b5
     a1   0  15  25  15  0
     a2   5  20   5  20  5
     a3  20   5   5   5 20
     ")


( cmhdemo1 <- as.matrix(cmhdemo1) )




# ----------
# linear association
cmhdemo2 <- read.table(header = TRUE, sep = "", text = "
         b1  b2   b3  b4  b5
     a1   2   5   8   8   8
     a2   2   8   8   8   5
     a3   5   8   8   8   2
     a4   8   8   8   5   2
     ")


( cmhdemo2 <- as.matrix(cmhdemo2) )




# ------------------------------------------------------------------------------
# basic analysis: Sieve Diagram
# ------------------------------------------------------------------------------

library(vcd)


sieve(cmhdemo1, shade = TRUE, main = "General association",
      gp = shading_sieve(interpolate = 0, lty = c("solid", "longdash")))



# sieve diagram show slightly weak linear association pattern
sieve(cmhdemo2, shade = TRUE, main = "General association",
      gp = shading_sieve(interpolate = 0, lty = c("solid", "longdash")))




# ------------------------------------------------------------------------------
# Standard Chisq Test
# ------------------------------------------------------------------------------

assocstats(cmhdemo1)



# ----------
# linear association data does NOT REJECT the independence hypothesis and have low Cramer's V

assocstats(cmhdemo2)





# ------------------------------------------------------------------------------
# Generalized Cochran-Mantel-Haensel tests
#   - Take the ordinal nature of a variable into account
#     Based on assigining numerical scores to the table categories; the default (table) scores treat the levels as equally spaced.
#     They generally have higher power when the pattern of association is determined by the order of an ordinal variable
#
#   - General Association:  When the row and column variables are both nominal (unordered), the only alternative hypothesis of interest is that
#     there is some association between the row and column variables. The CMH test statistic is similar to the (Pearson) Chi-Square and
#     Likelihood Ratio Chi-Square in the result from assocstats();  all have (r-1)(c-1) df.
#
#   - Row Mean Scores Differ:  If the column variable is ordinal, assigning scores to the column variables can be expressed
#     as a test of whether these means differe over the rows of the table, with r - 1 df.
#     This is analogous to the Kruskal-Wallis non-parametric test (ANOVA based on rank scores)
#
#   - Column Mean Scores Differ:  refer to "Row Mean Scores Differ", assigning scores to the row variable
#
#   - Nonzero Correlation (Linear Association):  When both row and column variables are ordinal, we could assign scores
#     to both variables and compute the correaltion (r),
#     giving Spearman's rank correlation coefficient.
#     The CMH Chi-square is equal to (N-1) * r^2, where N is the total sample size.
#     The test is most sensitive to a pattern where the row mean score changes linearly over the rows.
# ------------------------------------------------------------------------------


library(vcdExtra)


# The Chi-squared values for non-zero correlation and difference row mean scores are exactly zero because the row means are all equal.
# Only the general associatoin test shows that A and B are associated


CMHtest(cmhdemo1)



# ----------
# Note that the X^2 calues for the row means and non-zero correlation test from CMHtest() are very similar.
# But the correlation test is more highly significant since it is based on just one degree of freedom

CMHtest(cmhdemo2)


