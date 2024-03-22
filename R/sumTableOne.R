#' summary table one from tableone::CreateTableOne function
#'
#' Convert results from tableone::CreateTableOne
#' @param model a linear model object
#' @param n1 This is the first line you want to get from your summary table. If not specified, it is automatically set when taking your function
#' @param n2 This is the second line to which you want to get from summary table. If not specified, it is automatically set when taking your function
#' @param latex whether to export your result in latex form
#' @param toClip whether to export your result to clipboard. This function is still under construction. Don't change it.
#' @param pType whether to export your original P, defult to "mark", another option is "value"
#' @param ... other elements inherited from write.table
#' @export
#' @example demo/sumLM_demo.R
#' @details
#' In academic paper, only one or two lines of regression tables were shown rather than the whole table. Since we are only interested in the specific exposure. Thus, n1 stands for the line started from which we want to extract results. n2 stands for the line to which we want to extract. Normally, you do not need to change them since this package take the first independent variable in your regression model as the variable you are interested in. It will detect which line to take from the final table.
sumTableOne <- function(tableOne, latex = F, toClip = F, pType = "mark", ...){
  # clean the table header
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
  makeNamesTableOneExcel <- function(x){ # This is for table one output to excel
    a <- x[1,] %>%
      str_replace_all(pattern = " ", replacement  = "")
    a[which(a != "")] <- paste0("(n = ", a[which(a != "")], ")")
    # x <- rbind(paste0(colnames(x), a), x)
    colnames(x) <- paste0(colnames(x), a)
    x <- x[-1,]
    # x <- rbind(paste0(colnames(x), a),x)
    x <- data.frame(Variable = row.names(x), x, row.names = NULL, check.names = F)
    names(x)[which(names(x) == "p")] <- "P"
    return(x)
  }

  ## This is previous function created
  changeLevelTwoFactor <- function(df){
    df_con <- df[str_detect(df$Variable, "mean|median"),]
    # rownames(df_con) <- 1:nrow(df_con)
    df_cat <- df[!str_detect(df$Variable, "mean|median"),]
    # rownames(df_cat) <- 1:nrow(df_cat)
    index <- which(str_detect(df_cat$Variable, "\\%"))
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