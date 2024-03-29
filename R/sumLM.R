#' @templateVar class lm
#' @template titleDescSumReg
#'
#' @param model a linear model object
#' @template paramN1N2P
#' @template paramLatexToClip
#' @template paramUnusedDots
#' @export
#' @return return a tibble of regression table
#' @example demo/sumLM_demo.R
#' @details
#' In academic paper, only one or two lines of regression tables were shown rather than the whole table. Since we are only interested in the specific exposure. Thus, n1 stands for the line started from which we want to extract results. n2 stands for the line to which we want to extract. Normally, you do not need to change them since this package take the first independent variable in your regression model as the variable you are interested in. It will detect which line to take from the final table.


sumLM <- function(model,n1 = NULL,n2 = NULL,latex = TRUE,toClip = FALSE,pType = "mark", ...){
  target <- all.vars(as.formula(model$call[[2]]))[2]
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
      pvalue.4dPre = sprintf("%.4f", p.value),
      pvalue.4d = case_when(
        pvalue.4dPre == "0.0000" ~ "< 0.0001",
        TRUE ~ pvalue.4dPre
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

  if(toClip == TRUE){
    if(.Platform$OS.type == "windows"){
      write.table(x = res, file = "clipboard", quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
    }
    if(.Platform$OS.type == "unix"){
      clip <- pipe("pbcopy", "w")
      write.table(res, file=clip, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
      close(clip)
    }
  }
  warning("This function is deprecated, use sumReg instead")
  return(res)
}






