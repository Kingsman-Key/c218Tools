
# categorical outcome
model <- glm("Sepal.Length~ Sepal.Width", data = iris)
res <- sumReg(model)
# continuous outcome
input <- mtcars
am.data = glm(formula = am ~ cyl + hp + wt, data = input, family = "binomial")
table(mtcars$am)
res <- sumGLM(am.data)
res <- sumReg(am.data)
