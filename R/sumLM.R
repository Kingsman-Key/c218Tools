#' summary linear model
#'
#' Convert results from linear model. This function take the first variable in independent variables as the target variable.
#' @param model a linear model object
#' @param n1 This is the first line you want to get from your summary table. If not specified, it is automatically set when taking your function
#' @param n2 This is the second line to which you want to get from summary table. If not specified, it is automatically set when taking your function
#' @param latex whether to export your result in latex form
#' @param toClip whether to export your result to clipboard. This function is still under construction. Don't change it.
#' @param pType whether to export your original P, defult to "mark", another option is "value"
#' @param ... other elements inherited from write.table
#' @export
#' @example examples/sumLM_demo.R
#' @details
#' In academic paper, only one or two lines of regression tables were shown rather than the whole table. Since we are only interested in the specific exposure. Thus, n1 stands for the line started from which we want to extract results. n2 stands for the line to which we want to extract. Normally, you do not need to change them since this package take the first independent variable in your regression model as the variable you are interested in. It will detect which line to take from the final table.

sumLM <- function(model,n1 = 1,n2 = 2,latex = T,toClip = F,pType = "mark", ...){
  target <- all.vars(as.formula(model$call[[2]]))[2]
  data <- model[["model"]]
  if(class(data[[target]]) == "numeric"){
    n1 <- 2
    n2 <- 2
  }
  if(class(data[[target]]) == "character"|class(data[[target]]) == "factor"){
    n2 <- c218Tools::detectTargetLevels(target = target, data = data)
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
      dplyr::select(term, betase.mark.latex) %>%
      dplyr::slice(n1, n2)
  },
  {
    res %>% # value
      dplyr::select(term, betase.s1, pvalue.4d) %>%
      dplyr::slice(n1, n2)
  },
    res %>% # value
      dplyr::select(term, betase.mark.excel) %>%
      dplyr::slice(n1, n2)
  )
  if(n1 == 1){
    res[1,] <- "Ref."
  }
  if(toClip == T){
    if(.Platform$OS.type == "windows"){
      write.table(x = res, file = "clipboard", quote = F, sep = "\t")
    }
    if(.Platform$OS.type == "unix"){
      clip <- pipe("pbcopy", "w")
      write.table(res, file=clip, quote = F, sep = "\t")
      close(clip)
    }
  }
  return(res)
}





