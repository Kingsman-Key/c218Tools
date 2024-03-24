#' @templateVar class multinom
#' @template titleDescSumReg
#'
#' @param model a multinom model object
#' @template paramN1N2P
#' @template paramLatexToClip
#' @template paramUnusedDots
#' @seealso [write.table()] [tidy()]
#' @export
#' @example demo/sumMULTI_demo.R
#' @details
#' In academic paper, only one or two lines of regression tables were shown rather than the whole table. Since we are only interested in the specific exposure. Thus, n1 stands for the line started from which we want to extract results. n2 stands for the line to which we want to extract. Normally, you do not need to change them since this package take the first independent variable in your regression model as the variable you are interested in. It will detect which line to take from the final table.


sumReg.multinom <- function(model,n1 = NULL,n2 = NULL,latex = T,toClip = F,pType = "mark", ...){
  target <- all.vars(as.formula(model$call[[2]]))[2]
  outcome <- all.vars(as.formula(model$call[[2]]))[1]
  data <- get(model[["call"]][["data"]])
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


  res <- broom::tidy(x = model, exponentiate = T, conf.int = T) %>%
    dplyr::mutate(
      beta = base::sprintf("%.2f", estimate),
      up = base::sprintf("%.2f", conf.high),
      low = base::sprintf("%.2f", conf.low),
      or = base::sprintf("%.2f", estimate),
      or95.s1 = paste0(or, " (", low, ", ", up, ")"),
      se = base::sprintf("%.2f", std.error),
      betase.s1 = paste0(beta, " (", se, ")")
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
      ),
      pvalue.4dPre = sprintf("%.4f", p.value),
      pvalue.4d = case_when(
        pvalue.4dPre == "0.0000" ~ "< 0.0001",
        TRUE ~ pvalue.4dPre
      )
    )
  if(latex == T & pType == "mark"){ # determine which part should be exported
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
  return(res)
}
