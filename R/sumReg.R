#' @templateVar class regression
#' @template titleDescSumReg
#'
#' @param model a model object
#' @template paramN1N2P
#' @template paramLatexToClip
#' @template paramUnusedDots
#' @template paramDigits
#' @seealso [broom::tidy()]
#' @export
#' @return return a tibble of regression table
#' @example demo/sumReg_demo.R
#' @details
#' In academic paper, only one or two lines of regression tables were shown rather than the whole table. Since we are only interested in the specific exposure. Thus, n1 stands for the line started from which we want to extract results. n2 stands for the line to which we want to extract. Normally, you do not need to change them since this package take the first independent variable in your regression model as the variable you are interested in. It will detect which line to take from the final table.



sumReg <- function(model, n1 = NULL,n2 = NULL,latex = TRUE,toClip = FALSE,pType = "mark", digits = 2, pDigits = 4, ...){
  UseMethod("sumReg")
}
