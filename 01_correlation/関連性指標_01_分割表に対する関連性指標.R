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
# Cramer's V and Pearson's Chi-square value
# ------------------------------------------------------------------------------

assocstats(cmhdemo1)


assocstats(cmhdemo2)




