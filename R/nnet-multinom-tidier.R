#' @templateVar class multinom
#' @template titleDescSumReg
#'
#' @param model a multinom model object
#' @template paramN1N2P
#' @template paramLatexToClip
#' @template paramUnusedDots
#' @template paramDigits
#' @template paramAdjustOutcome
#' @seealso [utils::write.table()] [broom::tidy()]
#' @return @return A [tibble::tibble()] with information about model components.
#' @export
#' @example demo/nnet-multinom-tidier_demo.R
#' @details
#' In academic paper, only one or two lines of regression tables were shown rather than the whole table. Since we are only interested in the specific exposure. Thus, n1 stands for the line started from which we want to extract results. n2 stands for the line to which we want to extract. Normally, you do not need to change them since this package take the first independent variable in your regression model as the variable you are interested in. It will detect which line to take from the final table.

sumReg.multinom <- function(model,n1 = NULL,n2 = NULL,latex = TRUE,toClip = FALSE,pType = "mark", digits = 2, pDigits = 4, regressionTableOnly = T, ...){
  target <- all.vars(model[["terms"]])[2]
  outcome <- all.vars(model[["terms"]])[1]
  data <- model.frame(model)
  # judge n1 and n2

  n1n2BothNull <- is.null(n1) & is.null(n2)
  n1n2OneNull <- (!is.null(n1) & is.null(n2)) | (is.null(n1) & !is.null(n2))
  if(n1n2BothNull|n1n2OneNull){
    targetIsNumericOrLogical <- is.numeric(data[[target]])|is.logical(data[[target]])
    if(targetIsNumericOrLogical){
      n1 <- 2
      n2 <- c218Tools::detectTargetLevels(target = target, data = data)
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

  digitsToApply <- paste0("%.", digits, "f")
  pDigitsToApply <- paste0("%.", pDigits, "f")
  res <- broom::tidy(x = model, exponentiate = T, conf.int = T) %>%
    dplyr::mutate(
      beta = base::sprintf(digitsToApply, estimate),
      up = base::sprintf(digitsToApply, conf.high),
      low = base::sprintf(digitsToApply, conf.low),
      or = base::sprintf(digitsToApply, estimate),
      or95.s1 = paste0(or, " (", low, ", ", up, ")"),
      se = base::sprintf(digitsToApply, std.error),
      betase.s1 = paste0(beta, " (", se, ")"),
      pvalue.4dPre = sprintf(pDigitsToApply, p.value),
      pvalue.4d = case_when(
        pvalue.4dPre == paste0("0.", paste0(rep("0", pDigits), collapse = "")) ~ paste0("< 0.", paste0(rep("0", pDigits-1), collapse = ""), "1"),
        TRUE ~ pvalue.4dPre
      )
    ) %>%
    dplyr::mutate(
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
      dplyr::select(y.level, term, or95.mark.latex)
  },
  {
    res %>% # value
      dplyr::select(y.level, term, or95.s1, pvalue.4d)
  },
  {
    res %>% # value
      dplyr::select(y.level, term, or95.mark.excel)
  }
  )
  res <- res %>%
    tidyr::pivot_wider(data = ., id_cols = term, names_from = y.level, values_from = -c(y.level, term)) %>%
    dplyr::slice(n1:n2)
  if(n1 == 1){
    res[1,which(str_detect(names(res), "or"))] <- "Ref."
    res[1,which(str_detect(names(res), "pvalue"))] <- "-"
  }

  if(toClip == TRUE){
    if(.Platform$OS.type == "windows"){
      write.table(x = res, file = "clipboard", quote = FALSE, sep = "\t", ...)
    }
    if(.Platform$OS.type == "unix"){
      clip <- pipe("pbcopy", "w")
      write.table(x = res, file=clip, quote = FALSE, sep = "\t", ...)
      close(clip)
    }
  }

  colPercent <- matrix(sprintf("%.2f",prop.table(table(data[[target]], data[[outcome]]), margin = 2)*100), nrow = length(table(data[[target]])))
  freq <- table(data[[target]], data[[outcome]])
  des <- paste0(freq, " (", colPercent, ")") %>%
    matrix(., nrow = length(table(data[[target]])))

  res[] <- lapply(res[], as.character)
  tableName <- names(res)

  if(regressionTableOnly){
    return(res)
  }else if(!regressionTableOnly){
    resList <- list(regressionTable = res, model = "multinom", descriptionStatistics = des, tableName = tableName)
    class(resList) <- "sumReg"
    return(resList)
  }
}
