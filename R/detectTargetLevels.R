#' detect target levels
#'
#' @param target your target exposure
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




#' detect outcome levels
#'
#' @param outcome your outcome
#' @param data your data
#' @export
detectOutcomeLevels <- function(outcome,data){
  if(is.numeric(data[[outcome]])){
    outcomeCategory <- "continuous"
    return(outcomeCategory)
  }else if(is.character(data[[outcome]])){
    outcomeCategory <- "categorical"
    return(outcomeCategory)
  }else if(is.factor(data[[outcome]])){
    outcomeCategory <- "categorical"
    return(outcomeCategory)
  }
}
