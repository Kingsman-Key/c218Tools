## code to prepare `DATASET` dataset goes here


#' VitaminD data
#'
#' A subset of data from the World Health Organization Global Tuberculosis
#' Report ...
#'
#' @format ## `vd`
#' A data frame with 7,240 rows and 60 columns:
#' @
#' \describe{
#'   \item{country}{Country name}
#'   \item{iso2, iso3}{2 & 3 letter ISO country codes}
#'   \item{year}{Year}
#'   ...
#' }
#' @source <https://catalog.data.gov/dataset/photochemical-smog-and-vitamin-d-deficiency>
"who"




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

