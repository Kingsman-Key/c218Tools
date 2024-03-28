#' @templateVar class lm
#' @template titleDescSumReg
#'
#' @param model a linear model object
#' @template paramN1N2P
#' @template paramLatexToClip
#' @template paramUnusedDots
#' @template paramDigits
#' @seealso [write.table()] [tidy()]
#' @export
#' @return return a tibble of regression table
#' @example demo/sumLM_demo.R
#' @details
#' In academic paper, only one or two lines of regression tables were shown rather than the whole table. Since we are only interested in the specific exposure. Thus, n1 stands for the line started from which we want to extract results. n2 stands for the line to which we want to extract. Normally, you do not need to change them since this package take the first independent variable in your regression model as the variable you are interested in. It will detect which line to take from the final table.


sumReg.lm <- function(model,n1 = NULL,n2 = NULL,latex = TRUE,toClip = FALSE,pType = "mark", digits = 2, pDigits = 4, ...){
  target <- all.vars(as.formula(model$call[[2]]))[2]
  data <- model[["model"]]
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
  res <- model %>%
    broom::tidy(., conf.int =T) %>%
    dplyr::mutate(
      beta = sprintf("%.2f", estimate),
      up = sprintf("%.2f", conf.high),
      low = sprintf("%.2f", conf.low),
      # or95 = paste0(or, " (", low, ", ", up, ")"),
      se = sprintf("%.2f", std.error),
      betase.s1 = paste0(beta, " (", se, ")")
    ) %>%
    dplyr::mutate(
      betase.mark.latex = case_when(  # generate all the possible results that are needed
        p.value < 0.05 & p.value >= 0.01 ~ paste0(betase.s1, " ^$\\\\ast$^"),
        p.value < 0.01 & p.value >=0.001 ~ paste0(betase.s1, " ^$\\\\dag$^"),
        p.value < 0.001 ~ paste0(betase.s1, " ^$\\\\ddag$^"),
        TRUE ~ betase.s1
      ),
      betase.mark.excel = case_when(
        p.value < 0.05 & p.value >= 0.01 ~ paste0(betase.s1, "*"),
        p.value < 0.01 & p.value >=0.001 ~ paste0(betase.s1, "$"),
        p.value < 0.001 ~ paste0(betase.s1, "#"),
        TRUE ~ betase.s1
      ),
      pvalue.4dPre = sprintf(pDigitsToApply, p.value),
      pvalue.4d = case_when(
        pvalue.4dPre == paste0("0.", paste0(rep("0", pDigits), collapse = "")) ~ paste0("< 0.", paste0(rep("0", pDigits-1), collapse = ""), "1"),
        TRUE ~ pvalue.4dPre
      )
    )
  if(latex == TRUE & pType == "mark"){ # determine which part should be exported
    type <- "latexMark"
  }else if(pType == "value"){
    type <- "value"
  }else if(latex == FALSE & pType == "mark"){
    type <- "excelMark"
  }
  index <- which(type %in% c("latexMark", "value", "excelMark"))
  res <- switch(index, {
    res %>% #latex mark
      dplyr::select(term, betase.mark.latex) %>%
      dplyr::slice(n1:n2)
  },
  {
    res %>% # value
      dplyr::select(term, betase.s1, pvalue.4d) %>%
      dplyr::slice(n1:n2)
  },
  res %>% # value
    dplyr::select(term, betase.mark.excel) %>%
    dplyr::slice(n1:n2)
  )
  if(n1 == 1){
    res[1,] <- "Ref."
  }

  if(toClip == T){
    if(.Platform$OS.type == "windows"){
      write.table(x = res, file = "clipboard", quote = F, sep = "\t", row.names = F, col.names = F)
    }
    if(.Platform$OS.type == "unix"){
      clip <- pipe("pbcopy", "w")
      write.table(res, file=clip, quote = F, sep = "\t", row.names = F, col.names = F)
      close(clip)
    }
  }
  res[] <- lapply(res[], as.character)
  class(res) <- c("tbl_df", "tbl", "data.frame", "sumReg")
  return(res)
}






