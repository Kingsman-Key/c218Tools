#' @templateVar class coxph
#' @template titleDescSumReg
#'
#' @param model a coxph model object
#' @template paramN1N2P
#' @template paramLatexToClip
#' @template paramUnusedDots
#' @template paramDesc
#' @template paramDigits
#' @export
#' @return return a tibble of regression table
#' @example demo/sumGLM_demo.R
#' @details
#' In academic paper, only one or two lines of regression tables were shown rather than the whole table. Since we are only interested in the specific exposure. Thus, n1 stands for the line started from which we want to extract results. n2 stands for the line to which we want to extract. Normally, you do not need to change them since this package take the first independent variable in your regression model as the variable you are interested in. It will detect which line to take from the final table.


sumReg.coxph <- function(model ,n1 = NULL,n2 = NULL,latex = TRUE,toClip = FALSE,pType = "mark", desc = FALSE, digits = 2, pDigits = 4, ...){
  target <- all.vars(as.formula(model[["formula"]]))[3]
  data <- get(model[["call"]][["data"]])


  # judge n1 and n2

  n1n2BothNull <- is.null(n1) & is.null(n2)
  n1n2OneNull <- (!is.null(n1) & is.null(n2)) | (is.null(n1) & !is.null(n2))
  if(n1n2BothNull|n1n2OneNull){
    targetIsNumeric <- is.numeric(data[[target]])
    if(targetIsNumeric){
      n1 <- 1
      n2 <- 1
    }
    targetIsCharacterOrFactor <- is.character(data[[target]])|is.factor(data[[target]])
    if(targetIsCharacterOrFactor){
      n1 <- 1
      n2 <- c218Tools::detectTargetLevels(target = target, data = data) - 1
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
    broom::tidy(x = ., exponentiate = T, conf.int = T) %>%
    dplyr::mutate(
      beta = sprintf(digitsToApply, estimate),
      up = sprintf(digitsToApply, conf.high),
      low = sprintf(digitsToApply, conf.low),
      or = sprintf(digitsToApply, estimate),
      or95.s1 = paste0(or, " (", low, ", ", up, ")"),
      se = sprintf(digitsToApply, std.error),
      betase.s1 = paste0(beta, " (", se, ")"),
      pvalue.4dPre = sprintf(pDigitsToApply, p.value),
      pvalue.4d = case_when(
        pvalue.4dPre == paste0("0.", paste0(rep("0", pDigits), collapse = "")) ~ paste0("< 0.", paste0(rep("0", pDigits-1), collapse = ""), "1"),
        TRUE ~ pvalue.4dPre
      )
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

    res <- res %>%
      dplyr::select(term, contains("or95"), pvalue.4d)

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

  # if(n1 == 1){
  #   res[1,] <- "Ref."
  # }
  ## generate description data
  if(desc == TRUE){
    colPercent <- matrix(sprintf("%.2f",prop.table(table(data[[target]], data[[outcome]]), margin = 2)*100), nrow = length(table(data[[target]])))
    freq <- table(data[[target]], data[[outcome]])
    des <- paste0(freq, " (", colPercent, ")") %>%
      matrix(., nrow = length(table(data[[target]])))
    res <- cbind(res[,1], des, res[,-1])
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
  res[] <- lapply(res[], as.character)
  class(res) <- c("tbl_df", "tbl", "data.frame", "sumReg")
  return(res)
}

