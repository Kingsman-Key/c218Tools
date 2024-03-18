## code to prepare `DATASET` dataset goes here

conOutCome <- data.frame( # continuous outcome table conceive
  V1 =c("Vitamin D", "Vitamin D < 10", "Vitamin D > 10"),
  betaSe = c("1 (1)", "Ref.", "2 (2)"),
  p = c("< 0.0001", "-", "0.02")
)

binOutCome <- data.frame( # binary outcome table conceive
  V1 =c("Vitamin D", "Vitamin D < 10", "Vitamin D > 10"),
  V1 =c("Vitamin D", "Vitamin D < 10", "Vitamin D > 10"),
  V1 =c("Vitamin D", "Vitamin D < 10", "Vitamin D > 10"),
  betaSe = c("1 (1)", "Ref.", "2 (2)"),
  p = c("< 0.0001", "-", "0.02")
)

multOutCome <- data.frame( # multnome outcome table conceive
  V1 =c("Vitamin D", "Vitamin D < 10", "Vitamin D > 10"),
  betaSe = c("1 (1)", "Ref.", "2 (2)"),
  p = c("< 0.0001", "-", "0.02")
)

library(MASS)
library(tidyverse)
example(birthwt)
multOutCome <- nnet::multinom(low ~ ., bwt) %>%
  c218Tools::sumMULTI()


usethis::use_data(conOutCome, binOutCome, multOutCome, overwrite = TRUE)

