rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

pretest <- c(rep("pass", 7), rep("fail", 13))

posttest <- c(rep("pass", 6), rep("fail", 1), rep("pass", 8), rep("fail", 5))


( df <- data.frame(id = seq(1, 20, by = 1), pretest = pretest, posttest = posttest) )


head(df)



# ----------
table(pretest)

table(posttest)


( tab <- t(matrix(c(table(pretest), table(posttest)), nrow = 2)) )


row.names(tab) <- c("pretest", "posttest")

colnames(tab) <- c("fail", "pass")

tab



# ----------

( df2 <- data.frame(tab) %>% mutate(all = pass + fail, pass_ratio = round(pass / (pass + fail), 3)) )

head(df2)



# -->
# It seems that pass ratio in posttest is incresed from pretest




# ------------------------------------------------------------------------------
# prop.test
# ------------------------------------------------------------------------------

# Number of success --> pass pretest and posttest
x <- c(7, 14)


# Number of trials --> all objective for pretest and posttest
n <- c(20, 20)




# ----------
# Null Hypothesis:  Proportion of pass is NOT DIFFERENT between pretest and posttest
# Alternative Hypothesis:  Proportion of pass is DIFFERENT between pretest and posttest


( output1 <- prop.test(x = x, n = n, alternative = c("two.sided")) )


# p.value <- 0.05 indicating that Null Hypothesis IS REJECTED !!
output1$p.value




# ------------------------------------------------------------------------------
# Chi-Squared Test:  [posttest, pretest]  is relevant to [pass, fail]
# ------------------------------------------------------------------------------

# Null Hypothesis:  [posttest, pretest] and [pass, fail] are INDEPENDENT
# Alternative Hypothesis:  [posttest, pretest] and [pass, fail] are NOT INDEPENDENT

( output2 <- chisq.test(tab) )


# marginally NOT REJECTED
output2$p.value



# ------------------------------------------------------------------------------
# Fisher Exact test for [posttest, pretest]  vs [pass, fail]
# ------------------------------------------------------------------------------

# Null Hypothesis:  Proportion of pass is NOT DIFFERENT between pretest and posttest
# Alternative Hypothesis:  Proportion of pass is DIFFERENT between pretest and posttest

( output3 <- fisher.test(tab, simulate.p.value = TRUE, alternative = c("two.sided")) )


# marginally NOT REJECTED
output3$p.value







