#' summary table one from tableone::CreateTableOne function
#'
#' Convert results from tableone::CreateTableOne
#' @param tableOne a tableone object
#' @template paramLatexToClip
#' @param pType whether to export your original P, defult to "mark", another option is "value". When mark was selected. * stands for P < 0.05; $ stands for P < 0.01 # stands for P < 0.001. it was set default to `\\ast`, `\\dag`, `\\ddag`. to replace  $ # respectively.
#' @param ... other elements inherited from write.table
#' @seealso [utils::write.table()]
#' @export
#' @example demo/sumTableOne_demo.R
#' @details
#' In academic paper, only one or two lines of regression tables were shown rather than the whole table. Since we are only interested in the specific exposure. Thus, n1 stands for the line started from which we want to extract results. n2 stands for the line to which we want to extract. Normally, you do not need to change them since this package take the first independent variable in your regression model as the variable you are interested in. It will detect which line to take from the final table.
sumTableOne <- function(tableOne, latex = F, toClip = F, pType = "mark", ...){
  # clean the table header

  if(latex == T){
    res <- tableOne %>%
      makeNamesTableOneLatex(.) %>%
      changeLevelTwoFactor(.)
  }else if(latex == F){
    res <- tableOne %>%
      makeNamesTableOneExcel(.) %>%
      changeLevelTwoFactor(.)
  }
  if(toClip == T){
    if(.Platform$OS.type == "windows"){
      write.table(x = res, file = "clipboard", quote = F, sep = "\t", ...)
    }
    if(.Platform$OS.type == "unix"){
      clip <- pipe("pbcopy", "w")
      write.table(res, file=clip, quote = F, sep = "\t", ...)
      close(clip)
    }
  }
  return(res)
}

#' @templateVar class tableone
#' @template titleDescSumReg
#'
#' @param x a tableone model object
#' @export
#' @return return a tibble of regression table
#' @example demo/sumTableOne_demo.R
#' @details
#' In academic paper, only one or two lines of regression tables were shown rather than the whole table. Since we are only interested in the specific exposure. Thus, n1 stands for the line started from which we want to extract results. n2 stands for the line to which we want to extract. Normally, you do not need to change them since this package take the first independent variable in your regression model as the variable you are interested in. It will detect which line to take from the final table.


makeNamesTableOneLatex <- function(x){ # This is for table one output to latex
  a <- x[1,] %>%
    str_replace_all(pattern = " ", replacement  = "")
  a[which(a != "")] <- paste0("\n(n = ", a[which(a != "")], ")")
  # x <- rbind(paste0(colnames(x), a), x)
  colnames(x) <- paste0(colnames(x), a)
  x <- x[-1,]
  # x <- rbind(paste0(colnames(x), a),x)
  x <- data.frame(Variable = row.names(x), x, row.names = NULL, check.names = F)
  names(x)[which(names(x) == "p")] <- "P"
  return(x)
}

#' @templateVar class tableone
#' @template titleDescSumReg
#'
#' @param x a tableone object
#' @return return a tibble of regression table
#' @example demo/sumTableOne_demo.R
#' @export
#' @details
#' In academic paper, only one or two lines of regression tables were shown rather than the whole table. Since we are only interested in the specific exposure. Thus, n1 stands for the line started from which we want to extract results. n2 stands for the line to which we want to extract. Normally, you do not need to change them since this package take the first independent variable in your regression model as the variable you are interested in. It will detect which line to take from the final table.

makeNamesTableOneExcel <- function(x){ # This is for table one output to excel
  a <- x[1,] %>%
    stringr::str_replace_all(pattern = " ", replacement  = "")
  a[which(a != "")] <- paste0("(n = ", a[which(a != "")], ")")
  # x <- rbind(paste0(colnames(x), a), x)
  colnames(x) <- paste0(colnames(x), a)
  x <- x[-1,]
  # x <- rbind(paste0(colnames(x), a),x)
  x <- data.frame(Variable = row.names(x), x, row.names = NULL, check.names = F)
  names(x)[which(names(x) == "p")] <- "P"
  return(x)
}


#' @templateVar class tableone
#' @template titleDescSumReg
#' @param df a tableone object
#' @return return a tibble of regression table
#' @example demo/sumTableOne_demo.R
#' @export
#' @details
#' In academic paper, only one or two lines of regression tables were shown rather than the whole table. Since we are only interested in the specific exposure. Thus, n1 stands for the line started from which we want to extract results. n2 stands for the line to which we want to extract. Normally, you do not need to change them since this package take the first independent variable in your regression model as the variable you are interested in. It will detect which line to take from the final table.
changeLevelTwoFactor <- function(df){
  df_con <- df[stringr::str_detect(df$Variable, "mean|median"),]
  df_con[stringr::str_detect(df$Variable, "mean"),2:ncol(df_con)] <- lapply(df_con[stringr::str_detect(df$Variable, "mean"),2:ncol(df_con)], function(x){
    x <- ifelse(!stringr::str_detect(x, "\\,"), stringr::str_replace(string = x, pattern = "\\(", replacement = "± "), x)
    x <- ifelse(!stringr::str_detect(x, "\\,"),stringr::str_replace(string = x, pattern = "\\)", replacement = ""), x)
    return(x)
  })
  # df_con[stringr::str_detect(df$Variable, "median"),2:ncol(df_con)] <- lapply(df_con[stringr::str_detect(df$Variable, "median"),2:ncol(df_con)], function(x){
  #   x <- stringr::str_replace(string = x, pattern = "\\[", replacement = "(")
  #   x <- stringr::str_replace(string = x, pattern = "\\]", replacement = ")")
  #   return(x)
  # })
  # rownames(df_con) <- 1:nrow(df_con)
  df_cat <- df[!stringr::str_detect(df$Variable, "mean|median"),]
  # rownames(df_cat) <- 1:nrow(df_cat)
  index <- which(stringr::str_detect(df_cat$Variable, "\\%"))
  res1 <- addEmptyRowsBeforeSpecificIndex(index = index, df = df_cat)
  res1$Variable <- c(res1$Variable[-1], "")
  emptyStringIndex <- grep(pattern = "^$", x = res1$Variable)
  emptyStringLogic <- res1$Variable == res1$Variable[]
  res1$Variable <- ifelse(!res1$Variable == "", res1$Variable, paste0("    ",res1$level))
  if(any(str_detect(names(res1), pattern = "^P$"))){
    # names(res1)[which(names(res1) == "p")] <- "P"
    res1$P <- dplyr::lead(res1$P)
  }
  # res1 <- res1[-1,]
  res <- rbind(df_con, res1)
  res <- res[,-which(names(res) == "level")]
  if(any(str_detect(names(res1), pattern = "test"))){
    res <- res[,-which(names(res) == "test")]
    names(res1)[which(names(res1) == "p")] <- "P"
  }
  res[] <- lapply(res[], function(x){
    x <- tidyr::replace_na(x, "")
  })
  return(res)
}


