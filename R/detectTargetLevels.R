#' detect target levels
#'
#' @param target your formula
#' @param data your data
#' @export
detectTargetLevels <- function(target,data){
  if(is.numeric(data[[target]])){
    n2 <- 2
    return(n2)
  }else if(is.character(data[[target]])){
    n2 <- length(unique(data[[target]]))
    return(n2)
  }else if(is.factor(data[[target]])){
    n2 <- levels(data[[target]])
    return(n2)
  }
}
