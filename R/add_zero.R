#' Add zero
#' @param x vector of any class
add_zero <- function(x) {
  
  ifelse(nchar(x) == 1, paste0("0", x), x)
  
}