#' Cumsum with reset
#' @param x numeric vector
#' @param reset integer specifying reset value (default is zero)
#' @return numeric vector of same length as x
#' 
cumsum_with_reset <- function(x, reset = 0) {
  
  n <- length(x)
  x[is.na(x)] <- 0
  y <- rep(NA, n)
  if (x[1] != reset) {
    y[1] <- x[1]
  } else {
    y[1] <- 0
  }
  
  for (i in 2:n) {
    if (x[i] != reset) {
      if (x[i - 1] != reset) {
        y[i] <- y[i - 1] + x[i]
      } else {
        y[i] <- x[i]
      }
    } else {
      y[i] <- 0
    }
  }
  return(y)
}