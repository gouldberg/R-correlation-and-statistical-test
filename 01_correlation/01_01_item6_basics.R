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
# data distribution
# ------------------------------------------------------------------------------


summary(item6_quant)


summary(item6_quali)




# ----------
MTS::MTSplot(t(item6_quant))


MTS::MTSplot(t(item6_quali))




# ----------
psych::describe(item6_quant)


psych::describe(item6_quali)




# ----------
psych::pairs.panels(item6_quant)


psych::pairs.panels(item6_quali)



