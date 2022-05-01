#' Summarise distribution
#' @param x vector of any class
#' 
summarise_distribution <- function(x) {
  
  if (is.numeric(x)) {
    return(summary(x))
  } else {
    return()
  }
  
}