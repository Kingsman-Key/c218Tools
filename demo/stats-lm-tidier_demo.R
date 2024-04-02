

# example code
model <- lm("Sepal.Length~ Sepal.Width", data = iris)
regTab <- c218Tools::sumReg(model = model, toClip = TRUE)

