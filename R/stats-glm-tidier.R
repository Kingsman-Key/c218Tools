#' @templateVar class glm
#' @template titleDescSumReg
#'
#' @param model a glm model object
#' @template paramN1N2P
#' @template paramLatexToClip
#' @template paramUnusedDots
#' @template paramDigits
#' @template paramAdjustOutcome
#' @seealso [write.table()] [broom::tidy()]
#' @export
#' @return return a tibble of regression table
#' \item{regressionTable}{regression table}
#' \item{outcomeCategory}{the category of outcome}
#' \item{tableName}{regression table name}
#' \item{descriptionStatistics}{Joint frequency distribution of exposure and outcome}
#' @example demo/stats-glm-tidier_demo.R
#' @details
#' In academic paper, only one or two lines of regression tables were shown rather than the whole table. Since we are only interested in the specific exposure. Thus, n1 stands for the line started from which we want to extract results. n2 stands for the line to which we want to extract. Normally, you do not need to change them since this package take the first independent variable in your regression model as the variable you are interested in. It will detect which line to take from the final table.


sumReg.glm <- function(model ,n1 = NULL,n2 = NULL,latex = TRUE,toClip = FALSE,pType = "mark", digits = 2, pDigits = 4, regressionTableOnly = T, ...){
  target <- all.vars(as.formula(model[["formula"]]))[2]
  outcome <- all.vars(as.formula(model[["formula"]]))[1]
  data <- model[["model"]]
  # judge n1 and n2

  n1n2BothNull <- is.null(n1) & is.null(n2)
  n1n2OneNull <- (!is.null(n1) & is.null(n2)) | (is.null(n1) & !is.null(n2))
  if(n1n2BothNull|n1n2OneNull){
    targetIsNumeric <- is.numeric(data[[target]])
    if(targetIsNumeric){
      n1 <- 2
      n2 <- 2
    }
    targetIsCharacterOrFactor <- is.character(data[[target]])|is.factor(data[[target]])
    if(targetIsCharacterOrFactor){
      n1 <- 1
      n2 <- c218Tools::detectTargetLevels(target = target, data = data)
    }
  }
  n1n2BothNotNull <- !is.null(n1) & !is.null(n2)
  if(n1n2BothNotNull){
    n1 <- n1
    n2 <- n2
  }
  if(n1n2OneNull){
    warning("for arguments n1 and n2, only one element is set, automatically unset them all")
  }
  outcome <- all.vars(as.formula(model[["formula"]]))[1]
  outcomeCategory <- c218Tools::detectOutcomeLevels(outcome = outcome, data = data)
  if(outcomeCategory == "continuous"){
    exponentiate <- FALSE
  }else if(outcomeCategory == "categorical"){
    exponentiate <- TRUE
  }
  digitsToApply <- paste0("%.", digits, "f")
  pDigitsToApply <- paste0("%.", pDigits, "f")
  res <- model %>%
    broom::tidy(x = ., exponentiate = exponentiate, conf.int = TRUE) %>%
    dplyr::mutate(
      beta = sprintf("%.2f", estimate),
      up = sprintf("%.2f", conf.high),
      low = sprintf("%.2f", conf.low),
      or = sprintf("%.2f", estimate),
      or95.s1 = paste0(or, " (", low, ", ", up, ")"),
      se = sprintf("%.2f", std.error),
      betase.s1 = paste0(beta, " (", se, ")"),
      pvalue.4dPre = sprintf(pDigitsToApply, p.value),
      pvalue.4d = case_when(
        pvalue.4dPre == paste0("0.", paste0(rep("0", pDigits), collapse = "")) ~ paste0("< 0.", paste0(rep("0", pDigits-1), collapse = ""), "1"),
        TRUE ~ pvalue.4dPre
      )
    ) %>%
    dplyr::mutate(
      # betase = case_when(
      # p.value < 0.05 & p.value >= 0.01 ~ paste0(betase.s1, "*"),
      # p.value < 0.01 & p.value >=0.001 ~ paste0(betase.s1, "$"),
      # p.value < 0.001 ~ paste0(betase.s1, "#"),
      # TRUE ~ betase.s1
      # ),
      or95.mark.latex = case_when(
        p.value < 0.05 & p.value >= 0.01 ~ paste0(or95.s1, " ^$\\\\ast$^"),
        p.value < 0.01 & p.value >=0.001 ~ paste0(or95.s1, " ^$\\\\dag$^"),
        p.value < 0.001 ~ paste0(or95.s1, " ^$\\\\ddag$^"),
        TRUE ~ or95.s1
      ),
      or95.mark.excel = case_when(
        p.value < 0.05 & p.value >= 0.01 ~ paste0(or95.s1, "*"),
        p.value < 0.01 & p.value >=0.001 ~ paste0(or95.s1, "$"),
        p.value < 0.001 ~ paste0(or95.s1, "#"),
        TRUE ~ or95.s1
      )

    )
  if(latex == TRUE & pType == "mark"){ # determine which part should be exported
    type <- "latexMark"
  }else if(pType == "value"){
    type <- "value"
  }else if(latex == F & pType == "mark"){
    type <- "excelMark"
  }
  index <- which(type == c("latexMark", "value", "excelMark"))
  res <- switch(index, {
    res %>% #latex mark
      dplyr::select(term, or95.mark.latex) %>%
      dplyr::slice(n1:n2)
  },
  {
    res %>% # value
      dplyr::select(term, or95.s1, pvalue.4d) %>%
      dplyr::slice(n1:n2)
  },
  {
    res %>% # value
      dplyr::select(term, or95.mark.excel) %>%
      dplyr::slice(n1:n2)
  }
  )
  if(n1 == 1 & pType == "mark"){
    res[1,2:ncol(res)] <- "Ref."
  }else if(n1 == 1 & pType == "value"){
    res[1,2] <- "Ref."
    res[1,3] <- "-"
  }
  ## generate description data
  if(outcomeCategory == "categorical" & targetIsCharacterOrFactor){
    colPercent <- matrix(sprintf("%.2f",prop.table(table(data[[target]], data[[outcome]]), margin = 2)*100), nrow = length(table(data[[target]])))
    freq <- table(data[[target]], data[[outcome]])
    des <- paste0(freq, " (", colPercent, ")") %>%
      matrix(., nrow = length(table(data[[target]])))
    # res <- cbind(res[,1], des, res[,-1])
  }else{
      des <- NULL
    }

  if(toClip == T){
    if(.Platform$OS.type == "windows"){
      write.table(x = res, file = "clipboard", quote = F, sep = "\t", ...)
    }
    if(.Platform$OS.type == "unix"){
      clip <- pipe("pbcopy", "w")
      write.table(x = res, file=clip, quote = F, sep = "\t", ...)
      close(clip)
    }
  }
  res[] <- lapply(res[], as.character)
  tableName <- names(res)

  if(regressionTableOnly == T){
    return(res)
  }else if(regressionTableOnly == F){
    resList <- list(regressionTable = res, model = "glm", outcomeCategory = outcomeCategory, tableName = tableName, descriptionStatistics = des)
    class(resList) <- "sumReg"
    return(resList)
  }
}



#' draw RCS
#' @param model your logistic model
#' @param knots the numerb of knots you want to use, default is 5
#' @param ... Other arguments of rms::ols function
#' @seealso [rms::ols()]
#' @example demo/stats-lm-rcs_demo.R
#' @returns a list of objects you might need in RCS plot

#' \item{fitPred}{This is the dataframe you need when plotting RCS}
#' \item{pNonLinear}{This is the P for nonlinear}
#' \item{minimumX}{This is the X of minimum. It is usually used in inflection point detection.}
#' \item{maximumX}{This is the X of maximum. It is usually used in inflection point detection.}
#' @export



regRcs.glm <- function(model, knots = 5, ...){
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

  fit <- rms::lrm(formula = formula(form), data = data, ...)
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
  if(length(maximumY) > 0){
    maximumX <- df %>%
      dplyr::filter(yhat == maximumY) %>%
      pull(1)
  }else{
    maximumX <- NULL
  }
  minimumY <- df %>%
    dplyr::filter(yhat - yhatLead >0 & yhat - yhatLag > 0) %>%
    pull(yhat)
  if(length(maximumY) > 0){
    minimumX <- df %>%
      dplyr::filter(yhat == minimumY) %>%
      pull(1)
  }else{
    minimumX <- NULL
  }
  resList <- list(fitPred = fitPred, pNonLinear = p_nonlinear, minimumX = minimumX, maximumX = maximumX)
  return(resList)
}






