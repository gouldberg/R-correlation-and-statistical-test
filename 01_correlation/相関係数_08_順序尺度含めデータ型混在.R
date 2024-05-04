rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

data <- data.frame(w = c(6, 7, 2, 5, 6, 3, 3, 4, 1, 2),
                   x = c(1, 2, 3, 6, 1, 4, 3, 4, 7, 2),
                   y = as.factor(c(3, 3, 1, 1, 2, 2, 2, 1, 2, 2)),
                   z = as.factor(c(2, 3, 2, 1, 1, 2, 1, 1, 3, 2)))



data


str(data)



# ----------
car::scatterplotMatrix(~ w + x + y + z, data = data)



# ------------------------------------------------------------------------------
# hetcor() by polychor package
# ------------------------------------------------------------------------------

# hetcor function select appropriate correlation automatically based on the data type
# (numeric, ordred factor)

library(polycor)


ans <- hetcor(data)

ans



# ----------
ans$correlation


