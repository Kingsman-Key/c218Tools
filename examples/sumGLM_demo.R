


# example code
input <- mtcars
am.data = glm(formula = am ~ cyl + hp + wt, data = input, family = "binomial")
res <- sumGLM(am.data)
