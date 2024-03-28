#' @templateVar class multinom
#' @template titleDescSumReg
#'
#' @param model a multinom model object
#' @template paramN1N2P
#' @template paramLatexToClip
#' @template paramUnusedDots
#' @template paramDigits
#' @seealso [utils::write.table()] [broom::tidy()]
#' @return @return A [tibble::tibble()] with information about model components.
#' @export
#' @example demo/sumMULTI_demo.R
#' @details
#' In academic paper, only one or two lines of regression tables were shown rather than the whole table. Since we are only interested in the specific exposure. Thus, n1 stands for the line started from which we want to extract results. n2 stands for the line to which we want to extract. Normally, you do not need to change them since this package take the first independent variable in your regression model as the variable you are interested in. It will detect which line to take from the final table.

sumReg.multinom <- function(model,n1 = NULL,n2 = NULL,latex = TRUE,toClip = FALSE,pType = "mark", digits = 2, pDigits = 4, ...){
  target <- all.vars(as.formula(model$call[[2]]))[2]
  outcome <- all.vars(as.formula(model$call[[2]]))[1]
  data <- get(model[["call"]][["data"]])
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
      beta = base::sprintf("%.2f", estimate),
      up = base::sprintf("%.2f", conf.high),
      low = base::sprintf("%.2f", conf.low),
      or = base::sprintf("%.2f", estimate),
      or95.s1 = paste0(or, " (", low, ", ", up, ")"),
      se = base::sprintf("%.2f", std.error),
      betase.s1 = paste0(beta, " (", se, ")"),
      pvalue.4dPre = sprintf(pDigitsToApply, p.value),
      pvalue.4d = case_when(
        pvalue.4dPre == paste0("0.", paste0(rep("0", pDigits), collapse = "")) ~ paste0("< 0.", paste0(rep("0", pDigits), collapse = ""), "1"),
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
  index <- which(type %in% c("latexMark", "value", "excelMark"))
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
    res[1,] <- "Ref."
  }

  if(toClip == TRUE){
    if(.Platform$OS.type == "windows"){
      write.table(x = res, file = "clipboard", quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
    }
    if(.Platform$OS.type == "unix"){
      clip <- pipe("pbcopy", "w")
      write.table(x = res, file=clip, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
      close(clip)
    }
  }
  res[] <- lapply(res[], as.character)
  class(res) <- c("tbl_df", "tbl", "data.frame", "sumReg")
  return(res)
}
