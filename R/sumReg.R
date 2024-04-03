#' @templateVar class regression
#' @template titleDescSumReg
#'
#' @param model a model object
#' @template paramUnusedDots
#' @seealso [broom::tidy()]
#' @export
#' @return return a tibble of regression table
#' @example demo/sumReg_demo.R
#' @details
#' In academic paper, only one or two lines of regression tables were shown rather than the whole table. Since we are only interested in the specific exposure. Thus, n1 stands for the line started from which we want to extract results. n2 stands for the line to which we want to extract. Normally, you do not need to change them since this package take the first independent variable in your regression model as the variable you are interested in. It will detect which line to take from the final table.



sumReg <- function(model, ...){
  UseMethod("sumReg", model)
}
