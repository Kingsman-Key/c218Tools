# example code
df <- iris
df <- within(df, {
Sepal.Width.2f <- NA_character_
Sepal.Width.2f[Sepal.Width < median(Sepal.Width)] <- "0"
Sepal.Width.2f[Sepal.Width >= median(Sepal.Width)] <- "1"
})

df$Sepal.Width.2f <- factor(df$Sepal.Width.2f, levels = c("0", "1"))
outcome <- "Sepal.Length"
exposure <- c("Sepal.Width", "Sepal.Width.2f")
covariate <- list(c("Petal.Length", "Petal.Width"), c("Species"))
regDisplay(outcome = outcome, exposure = exposure, covariate = covariate, data = df, regType = "lm")


