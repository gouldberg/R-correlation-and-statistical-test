rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

# Example 1
pretest <- c(rep("pass", 5), rep("hold", 2), rep("fail", 9), rep("hold", 4))

posttest <- c(rep("pass", 4), rep("hold", 2), rep("fail", 1), rep("pass", 6), rep("hold", 3), rep("fail", 4))


# ----------
# Example 2
pretest <- c(rep("pass", 2), rep("hold", 2), rep("fail", 12), rep("hold", 4))

posttest <- c(rep("pass", 4), rep("hold", 2), rep("fail", 1), rep("pass", 10), rep("hold", 3), rep("fail", 1))



# ----------
( df <- data.frame(id = seq(1, 20, by = 1), pretest = pretest, posttest = posttest) )


head(df)



# ----------
table(pretest)

table(posttest)


( tab <- t(matrix(c(table(pretest), table(posttest)), nrow = 3)) )


row.names(tab) <- c("pretest", "posttest")

colnames(tab) <- c("fail", "hold", "pass")

tab



# ----------

( df2 <- data.frame(tab) %>% mutate(all = pass + hold + fail, pass_ratio = round(pass / all, 3), hold_ratio = round(hold / all, 3)) )

head(df2)




# ------------------------------------------------------------------------------
# Chi-Squared Test:  [posttest, pretest]  is relevant to [pass, hold, fail]
# ------------------------------------------------------------------------------

# Null Hypothesis:  [posttest, pretest] and [pass, hold, fail] are INDEPENDENT
# Alternative Hypothesis:  [posttest, pretest] and [pass, hold, fail] are NOT INDEPENDENT

( output2 <- chisq.test(tab) )


# Rejected
output2$p.value



# ------------------------------------------------------------------------------
# Fisher Exact test for [posttest, pretest]  vs [pass, hold, fail]
# ------------------------------------------------------------------------------

# Null Hypothesis:  Proportion of pass, hold and fail are NOT DIFFERENT between pretest and posttest
# Alternative Hypothesis:  Proportion of pass, hold and fail are DIFFERENT between pretest and posttest

( output3 <- fisher.test(tab, simulate.p.value = TRUE, alternative = c("two.sided")) )


# Rejected
output3$p.value







