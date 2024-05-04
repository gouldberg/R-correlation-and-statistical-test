rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# check data
# ------------------------------------------------------------------------------

data(bock, package = "psych")


lsat6


dim(lsta6)


for(i in 1:dim(lsat6)[2]) print(table(lsat6[,i]))




# ------------------------------------------------------------------------------
# Tetrachoric correlation
# ------------------------------------------------------------------------------


library(psych)


tetrachoric(lsat6, smooth = TRUE)


# tetrachoric(x = lsat6[,c(1,2)], smooth = TRUE)



# -->
# tau:  the normality equivalent of the cutpoints

# for this data, polychoric correlation is same with that of tetrachoric
# meaning tetrachoric correlation is special case of polychoric correlation



polychoric(lsat6, smooth = TRUE)



