# example

data(mtcars)
input <- mtcars
am.data = glm(formula = am ~ mpg + hp + wt, data = input, family = "binomial")

regRcs(am.data)
res <- sumReg(am.data)
model <- am.data
knots <- 5
target <- all.vars(model[["terms"]])[2]  # This function can not promise to get the formula
outcome <- all.vars(model[["terms"]])[1]
formulaLengthLessThan3 <- length(all.vars(model[["terms"]])) < 3

data <- model[["model"]]
dd <<- rms::datadist(data) # <<- This function save object in function
options(datadist='dd')
rcsPart <- paste0("rms::rcs(", target, ",", knots, ")")

if(formulaLengthLessThan3){
  covariate <- NULL
  form <- paste0(outcome, "~", rcsPart)

}else{
  covariate <- all.vars(model[["terms"]])[3:length(all.vars(model[["terms"]]))]
  form <- paste0(outcome, "~", rcsPart, "+", paste0(covariate, collapse = "+"))
}

fit <- rms::lrm(formula = formula(form), data = data)
a <- stats::anova(fit) %>%
  as.data.frame()
p_nonlinear <- ifelse(a$P[[2]]==0,"<0.0001", round(a$P[[2]], 4))
fitPred <- rms::Predict(fit, name = target)
df <- data.frame(
  yhat = fitPred[["yhat"]],
  yhatLead = dplyr::lead(fitPred[["yhat"]]),
  yhatLag = dplyr::lag(fitPred[["yhat"]])
)
maximumY <- df %>%
  dplyr::filter(yhat - yhatLead >0 & yhat - yhatLag > 0) %>%
  pull(yhat)
is.null(maximumY)
length(maximumY) == 0
maximumX <- df %>%
  dplyr::filter(yhat == maximumY) %>%
  pull(1)
minimumY <- df %>%
  dplyr::filter(yhat - yhatLead >0 & yhat - yhatLag > 0) %>%
  pull(yhat)
minimumX <- df %>%
  dplyr::filter(yhat == minimumY) %>%
  pull(1)

resList <- list(fitPred = fitPred, pNonLinear = p_nonlinear, minimumX = minimumX, maximumX = maximumX)
return(resList)
