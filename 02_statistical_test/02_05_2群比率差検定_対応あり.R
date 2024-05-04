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
( tab2 <- as.matrix(xtabs(~ pretest + posttest, data = df)) )



# ----------

( df2 <- data.frame(tab) %>% mutate(all = pass + fail, pass_ratio = round(pass / (pass + fail), 3)) )

head(df2)



# -->
# It seems that pass ratio in posttest is incresed from pretest




# ------------------------------------------------------------------------------
# Fisher Exact test for [posttest, pretest]  vs [pass, fail]
# ------------------------------------------------------------------------------

# Null Hypothesis:  Proportion of pass is NOT DIFFERENT between pretest and posttest
# Alternative Hypothesis:  Proportion of pass is DIFFERENT between pretest and posttest

( output1 <- fisher.test(tab, simulate.p.value = TRUE, alternative = c("two.sided")) )


# marginally NOT REJECTED
output1$p.value



# ------------------------------------------------------------------------------
# MacNemar's chi-squared test for symmetry of rows and columns in a two-dimensional contingency table
# ------------------------------------------------------------------------------

( output2 <- mcnemar.test(tab2, correct = TRUE) )



# p.value = 0.046, indicating that Null Hypothesis is REJECTED
output2$p.value



# ------------------------------------------------------------------------------
# Sign Test
# ------------------------------------------------------------------------------

( x1 <- df %>% filter(pretest == "pass", posttest == "fail") %>% nrow() )


( x2 <- df %>% filter(pretest == "fail", posttest == "pass") %>% nrow() )


( x <- min(x1, x2) )


( n <- x + x2 )


( output3 <- binom.test(x = x, n = n, alternative = c("two.sided")) )


output3$p.value



# -->
# p.value = 0.039 indicating that Null Hypothesis is REJECTED


