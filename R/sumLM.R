#' summary linear model
#'
#' Convert results from linear model
#' @param form a linear model object
#' @param n1 This is the first line you want to get from your summary table
#' @param n2 This is the second line to which you want to get from summary table
#' @param data your data
#' @param latex whether to export your result in latex form
#' @param toClip whether to export your result to clipboard
#' @param pType whether to export your original P, defult to "mark", another option is "value"
#' @param ... other elements inherited from write.table
#' @export
sumLM <- function(form,n1 = 1,n2 = 2,data, latex = T,toClip = F,pType = "mark", ..){
  target <- all.vars(as.formula(form))[2]
  if(class(data[[target]]) == "numeric"){
    n1 <- 2
    n2 <- 2
  }
  if(class(data[[target]]) == "character"|class(data[[target]]) == "factor"){
    n2 <- c218Tools::detectTargetLevels(target = target, data = data)
  }
  res <- lm(formula = as.formula(form), data = data) %>%
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
  }
  index <- which(type %in% c("latexMark", "value"))
  res <- switch(index, {
    res %>% #latex mark
      dplyr::select(term, betase.mark.latex) %>%
      dplyr::slice(n1, n2)
  },
  {
    res %>% # value
      dplyr::select(term, betase.s1, pvalue.4d) %>%
      dplyr::slice(n1, n2)
  }
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





