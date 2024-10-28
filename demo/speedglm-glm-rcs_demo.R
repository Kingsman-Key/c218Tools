# example

data(mtcars)
input <- mtcars
am.data <- speedglm::speedglm(formula = am ~ mpg + hp + wt, data = input, family = binomial())

regRcs(am.data, ref.zero = T, fun = exp)
res <- sumReg(am.data)
