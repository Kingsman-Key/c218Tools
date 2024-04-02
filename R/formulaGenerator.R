#' Formula generator
#' @param outcome outcome of your regression.
#' @param exposure exposure you are interested in.
#' @param covariate covariate you want to adjust. If you want to generate formula with several batch of variables to adjust. please provide
#' @export
#' @example demo/formulaGenerator_demo.R
genForm <- function(outcome, exposure, covariate){
  if(!is.list(covariate)){
    form <- list()
    form[[1]] <- paste0(outcome, "~", exposure)
    form[[2]] <- paste0(outcome, "~", exposure, "+", paste0(covariate, collapse = "+"))
  }else if(is.list(covariate)){
    form <- list()
    form[[1]] <- paste0(outcome, "~", exposure)
    for (i in 1:length(covariate)) {
      form[[i+1]] <- paste0(outcome, "~", exposure, "+", paste0(unlist(covariate[1:i]), collapse = "+"))
    }
  }
  form <- unlist(form)
  return(form)
}
