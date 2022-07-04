#' Return in pct
#' @param x numeric vector
#' 
return_in_pct <- function(x) {
  
  {100 * (x - 1)} %>% 
    round(x = ., digits = 1)
  
}