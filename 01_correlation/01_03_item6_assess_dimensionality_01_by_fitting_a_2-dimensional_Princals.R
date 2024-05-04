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



# ----------
# convert to factors
dat <- item6_quali
dat$現代文 <- as.factor(dat$現代文)
dat$古典 <- as.factor(dat$古典)
dat$物理 <- as.factor(dat$物理)
dat$地学 <- as.factor(dat$地学)
str(dat)



# ----------
# Correlation among all variables, selecting appropriate method automatically
cor_all <- polycor::hetcor(data = dat, std.err = TRUE, ML = TRUE, use = "complete.obs")
cor_all



# ----------
cor_cont <- cor(item6_quant, method = "pearson")
cor_disc <- cor_all$correlations




# ------------------------------------------------------------------------------
# Assess dimensionality
#   - fitting a 2-dimensional Princals in order to get a picture of item associations in a 2D space.
# ------------------------------------------------------------------------------

library(Gifi)


prin <- princals(item6_quant)

prin2 <- princals(item6_quali)


prin

prin2



# ----------
# plot loadings

plot(prin, main = "Cont:  Loadings")


plot(prin2, main = "Disc:  Loadings")


