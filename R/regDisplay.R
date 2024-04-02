#' do regression in batch
#' @param outcome your outcome. It should only be an element.
#' @param exposure your exposure, it could be a vector or a list
#' @param covariate your covariate, it coule be a list with vectors
#' @param data your data
#' @param ... other arguments for geepack::glm such as id
#' @seealso [geepack::geeglm()]
#' @example demo/regDisplay_demo.R
#' @export

regDisplay <- function(outcome, exposure, covariate, data, regType = list("lm", "glm", "multinom", "coxph", "gee"), ...){
  ## check the input
  outcomeLengthMoreThan1 <- length(outcome) > 1
  exposureLengthMoreThan1 <- length(exposure) > 1
  exposureLengthIs1 <- length(exposure) == 1
  if(outcomeLengthMoreThan1){
    warning("Detecting the outcome more than 1. Only the first will be used")
    outcome <- outcome[1]
  }
  if(exposureLengthMoreThan1){
    form <- lapply(exposure, function(x){
      c218Tools::genForm(outcome = outcome, exposure = x, covariate = covariate)
    }) %>% unlist()
  }else if(exposureLengthIs1){
    form <- c218Tools::genForm(outcome = outcome, exposure = exposure, covariate = covariate)

  }
  res <- lapply(X = form, function(x){
    outcomeVector <- data[[all.vars(formula(x))[1]]]
    outcomeIsNumeric <- is.numeric(outcomeVector)
    outcomeIs2LevelFactor <- is.factor(outcomeVector) & length(levels(outcomeVector)) == 2
    outcomeIs3OrMoreLevelFactor <- is.factor(outcomeVector) & length(levels(outcomeVector)) >= 3
    outcomeIs2LevelCharacter <- is.character(outcomeVector) & length(unique(outcomeVector)) == 2
    outcomeIs3OrMoreLevelCharacter <- is.character(outcomeVector) & length(unique(outcomeVector)) >= 3
    outcomeIsLogical <- is.logical(outcomeVector)

    if(regType == "lm"){
      fit <- stats::lm(formula = formula(x), data = data)
      res <- sumReg(model = fit)
    }else if(regType == "glm" & outcomeIsNumeric){
      fit <- stats::glm(formula = formula(x), family = "binomial", data = data)
      res <- sumReg(model = fit)
    }else if(regType == "glm" & (outcomeIs2LevelFactor|outcomeIs2LevelCharacter|outcomeIsLogical)){
      fit <- stats::glm(formula = formula(x), data = data)
      res <- sumReg(model = fit)
    }else if(regType == "multinom"){
      fit <- nnet::multinom(formula = formula(x), data = data)
      res <- sumReg(model = fit)
    }else if(regType == "coxph"){
      fit <- survival::coxph(formula = formula(x), data = data)
      res <- sumReg(model = fit)
    }else if(regType == "gee" & outcomeIsNumeric){
      fit <- geepack::geeglm(formula = formula(x), data = data, id = id, )
      res <- sumReg(model = fit)
    }else if(regType == "gee" & (outcomeIs2LevelFactor|outcomeIs2LevelCharacter|outcomeIsLogical)){
      fit <- geepack::geeglm(formula = formula(x), data = data, family = "binomial")
      res <- sumReg(model = fit)
    }
  })
  exposureLength <- length(exposure)
  modelNum <- length(covariate) + 1
  resFnl <- cbnRegLst(exposureLength = exposureLength, ModelNum = modelNum, regList = res)
  return(resFnl)
}
