rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# check data
# ------------------------------------------------------------------------------

women


str(women)


plot(women)



# ------------------------------------------------------------------------------
# corrleation matrix by Pearson's Product of Moment
# ------------------------------------------------------------------------------

cor(women, method = c("pearson"))



# ------------------------------------------------------------------------------
# corrleation coefficient only
# ------------------------------------------------------------------------------

cor(women$height, women$weight, method = c("pearson"))






