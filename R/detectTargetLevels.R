#' detect target levels
#'
#' @param form your formula
#' @export
detectTargetLevels <- function(target,data){
  if(class(data[[target]]) == "numeric"){
    n2 <- 2
    return(n2)
  }else if(class(data[[target]]) == "character"){
    n2 <- length(unique(data[[target]]))
    return(n2)
  }else if(class(data[[target]]) == "factor"){
    n2 <- levels(data[[target]])
    return(n2)
  }
}
