rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------

setwd("C:\\Users\\kswad\\OneDrive\\ƒfƒXƒNƒgƒbƒv\\‹Zp—Í‹­‰»_“Œv‰ðÍ\\51_‰ðÍƒXƒNƒŠƒvƒg\\00_basics\\02_correlation")


item6_quant <- read.csv("item6_quant.csv", header = T, sep = ",")


item6_quali <- read.csv("item6_quali.csv", header = T, sep = ",")


str(item6_quant)


str(item6_quali)




# ------------------------------------------------------------------------------
# cross tab
# ------------------------------------------------------------------------------


table(item6_quali[,c("ŒÃ“T", "’nŠw")])




# ------------------------------------------------------------------------------
# tetrachoric correlation:  2(disc) * 2(disc)
# ------------------------------------------------------------------------------


library(polycor)


tetran <- polychor(x = item6_quali$ŒÃ“T, y = item6_quali$’nŠw, std.err = TRUE, ML = TRUE)


tetran



# ----------
tetran$rho


tetran$row.cuts


tetran$col.cuts


round(tetran$var, 4)




# -->
# variance is very small, no problem for estimation




# ------------------------------------------------------------------------------
# polychoric correlation:  n * m  (at least one of n and m have 3 or more categories)
# ------------------------------------------------------------------------------


table(item6_quali[,c("ŒÃ“T", "Œ»‘ã•¶")])



pola <- polychor(x = item6_quali$ŒÃ“T, y = item6_quali$Œ»‘ã•¶, std.err = TRUE, ML = TRUE)


pola



# ----------
pola$rho


pola$row.cuts


pola$col.cuts


# -->
# 2 cut point for "Œ»‘ã•¶"




# ----------
library(dplyr)
library(gtool1)


( tmp <- combinations(n = 4, r = 2, v = c("ŒÃ“T", "•¨—", "Œ»‘ã•¶", "’nŠw"), repeats.allowed = FALSE) )

( tmp <- tmp[-4,] )


sapply(1:nrow(tmp), 
       function(i) polychor(x = c(item6_quali[,tmp[i,1]]), y = c(item6_quali[,tmp[i,2]]), 
                            std.err = FALSE, ML = TRUE)
       )




# ------------------------------------------------------------------------------
# biserial correlation:  n (continuous) * m (discreate, only 2 categories)
# ------------------------------------------------------------------------------


bicle <- polycor::polyserial(x = item6_quali$‰pŒê, y = item6_quali$ŒÃ“T, std.err = TRUE, ML = TRUE)


bicle




# ----------
bicle$rho


bicle$cuts


bicle$var




# ------------------------------------------------------------------------------
# polyserial correlation:  n (continuous) * m (discreate, 3 or more categories)
# ------------------------------------------------------------------------------


polastar <- polycor::polyserial(x = item6_quali$‰pŒê, y = item6_quali$Œ»‘ã•¶, std.err = TRUE, ML = TRUE)


polastar



# ----------
polastar$rho


polastar$cuts


polastar$var

