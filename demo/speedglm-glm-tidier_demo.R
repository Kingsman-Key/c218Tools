# categorical outcome
model <- speedglm::speedglm("Sepal.Length~ Sepal.Width", data = iris)
res <- sumReg(model)
# continuous outcome
data(mtcars)
input <- mtcars
am.data = speedglm::speedglm(formula = am ~ cyl + hp + wt, data = input, family = binomial())
res <- sumReg(am.data)
