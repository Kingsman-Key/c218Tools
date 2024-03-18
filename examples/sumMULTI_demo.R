# example code
example(birthwt)
bwt.mu <- nnet::multinom(low ~ smoke, bwt)
res <- bwt.mu %>%
  c218Tools::sumMULTI()
