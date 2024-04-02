# examples

outcome <- "outcome"
exposure <- "exposure"
covariate <- c("a", "b")
genForm(outcome = outcome, exposure = exposure, covariate = covariate)


outcome <- "outcome"
exposure <- "exposure"
covariate <- list(c("a", "b"), c("c", "d"))
genForm(outcome = outcome, exposure = exposure, covariate = covariate)

outcome <- "outcome"
exposure <- c("exposure", "exposure2")
covariate <- list(c("a", "b"), c("c", "d"))
form <- lapply(exposure, function(x){
  genForm(outcome = outcome, exposure = x, covariate = covariate)
})

