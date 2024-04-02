# example code

data(birthwt, package = "MASS")
example(package = "MASS", birthwt)
bwt.mu <- nnet::multinom(low ~ smoke, bwt)
res <- c218Tools::sumReg(bwt.mu)






