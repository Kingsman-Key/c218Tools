# example code

model <- lm("Sepal.Length~ Sepal.Width", data = iris)
c218Tools::regRcs(model = model, knots = 5)

