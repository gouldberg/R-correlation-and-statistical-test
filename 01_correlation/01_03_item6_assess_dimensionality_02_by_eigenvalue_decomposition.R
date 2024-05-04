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
# Determining the number of factors:  eigenvalue decomposition (this means that we are fitting a PCA)
# ------------------------------------------------------------------------------

( evals1 <- eigen(cor_cont)$values )
( evals2 <- eigen(cor_disc)$values )



# ----------
# eigenvalues tell us how much variance is explained by each factor (or component by PCA)
scree(cor_cont, factors = FALSE)
scree(cor_disc, factors = FALSE)


round((cumsum(evals1) / sum(evals1))[1:5], 5)
round((cumsum(evals2) / sum(evals2))[1:5], 5)



# -->
# Elbow:  we hope to see a clear cut point which separates the systematic structure (rock) from the random structure (scree).
# We have a strongly dominating first factor

# Kaiser (1960) suggests that eignevalues hould be larger than 1.
# The rationale behind it is that if a factor is associated with an eigenvalue smaller than 1, it accounts for less variability than a single variable does.
# Here, We should retain 2 factors.
