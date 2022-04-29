#' Get this year
#' 
get_this_year <- function() {
  Sys.Date() %>% 
    substring(1, 4)
}

#' Month
#' @param x date vector of the format yyyy-mm-dd
#'  
month <- function(x) {
  
  substring(x, 6, 7)
  
}

#' One month ago
#' 
one_month_ago <- function() {
  
  Sys.Date() - 30

  
}

#' Three months ago
three_months_ago <- function() {
  
  Sys.Date() - 3 * 30
  
}