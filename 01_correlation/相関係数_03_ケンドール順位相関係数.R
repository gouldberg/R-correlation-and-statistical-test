rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------


boss1_hyouka_gyoseki <- c(5, 3, 4, 1, 6, 2, 7, 8)

boss1_hyouka_sougou <- c(4, 3, 5, 2, 6, 1, 8, 7)

boss2_hyouka_gyoseki <- c(8, 4, 3, 1, 5, 2, 7, 6)

boss2_hyouka_sougou <- c(6, 2, 1, 4, 3, 7, 8, 5)


buka_id <- seq(1, 8, by = 1)


( df <- data.frame(buka_id, boss1_hyouka_gyoseki, boss1_hyouka_sougou, boss2_hyouka_gyoseki, boss2_hyouka_sougou) )




# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------


library(psych)


# Pearson, Spearman, Kendall correlation (tau-a)

psych::pairs.panels(df[,2:ncol(df)], method = "pearson")

psych::pairs.panels(df[,2:ncol(df)], method = "spearman")

psych::pairs.panels(df[,2:ncol(df)], method = "kendall")



# ------------------------------------------------------------------------------
# Kendall's Rank Correlation (tau-a)
# ------------------------------------------------------------------------------

cor(boss1_hyouka_gyoseki, boss2_hyouka_gyoseki, method = "kendall")

cor(boss1_hyouka_sougou, boss2_hyouka_sougou, method = "kendall")



# ------------------------------------------------------------------------------
# corrleation coefficient only
# ------------------------------------------------------------------------------

cor(women$height, women$weight, method = c("pearson"))

cor(women$height, women$weight, method = c("spearman"))





