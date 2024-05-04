rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

file_path <- "http://www.sthda.com/sthda/RDoc/data/housetasks.txt"


housetasks <- read.delim(file_path, row.names = 1)


housetasks


str(housetasks)




# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

dt <- as.table(as.matrix(housetasks))



# ----------
library(gplots)

balloonplot(t(df), main = "housetasks", xlab = "", ylab = "", label = FALSE, show.margins = FALSE)



# ----------
library(graphics)

masaicplot(dt, shade = TRUE, las = 2, main = "housetasks")



# ----------
library(vcd)

assoc(head(dt, 5), shade = TRUE, las = 3)



# ------------------------------------------------------------------------------
# Compute Chi-Square statistics and Fisher's exact test
# ------------------------------------------------------------------------------

# Null Hypothesis:  The tasks and performers are INDEPENDENT
# Alternative Hypothesis:  The tasks and performers are NOT INDEPENDENT


output1 <- chisq.test(housetasks)


output2 <- fisher.test(housetasks, simulate.p.value = TRUE, alternative = c("two.sided"))



# ----------
# p.value is almost zero, indicating that tasks and performers are NOT INDEPENDENT

output1$p.value

output1$p.value

