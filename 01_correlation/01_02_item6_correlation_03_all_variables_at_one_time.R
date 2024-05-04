rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------

setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\02_correlation")


item6_quant <- read.csv("item6_quant.csv", header = T, sep = ",")


item6_quali <- read.csv("item6_quali.csv", header = T, sep = ",")


str(item6_quant)


str(item6_quali)




# ------------------------------------------------------------------------------
# convert to factors
# ------------------------------------------------------------------------------

dat <- item6_quali


dat$現代文 <- as.factor(dat$現代文)

dat$古典 <- as.factor(dat$古典)

dat$物理 <- as.factor(dat$物理)

dat$地学 <- as.factor(dat$地学)


str(dat)




# ------------------------------------------------------------------------------
# Correlation among all variables, selecting appropriate method automatically
# ------------------------------------------------------------------------------


cor_all <- polycor::hetcor(data = dat, std.err = TRUE, ML = TRUE, use = "complete.obs")


cor_all



round(cor_all$correlations, 3)




# ----------
# for reference:  by original data (before converting to factor)
# Note that integer variable are treated as "continuous" variable (only Pearson correlation is applied)

polycor::hetcor(data = item6_quali, std.err = TRUE, ML = TRUE, use = "complete.obs")




