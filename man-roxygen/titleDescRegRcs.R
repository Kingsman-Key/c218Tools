#' @title sum a(n) <%= class %> object
#'
#' @description sumReg summarizes information about the components of a model and turns it into a prediction dataframe that can be used to plot in ggplot1
#' For technical reasons, changing the variable type directly in the formula is not supported. For example:
#' df <- mtcars
#' model <- lm("disp ~ as.factor(carb)", data = df)
#' res <- c218Tools::sumReg(model = model)
#' The above code chunk would cause error for that the package can not detect the target variable levels.
#' Rather, Please do it before the model.
#' df <- mtcars
#' df$carb <- as.factor(df$carb)
#' model <- lm("disp~ carb", data = df)
#' res <- c218Tools::sumReg(model = model)
#'
#' @md
