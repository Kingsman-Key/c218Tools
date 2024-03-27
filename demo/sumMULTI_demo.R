# example code
library(MASS)
library(nnet)
example(birthwt)
bwt.mu <- nnet::multinom(low ~ smoke, bwt)
res <- c218Tools::sumMULTI(bwt.mu)
res <- c218Tools::sumReg(bwt.mu)






