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

#' Season 2021 start
season_2021_start <- function() {
  
  as.Date("2020-07-30")
  
}

#' Season 2021 end
season_2021_end <- function() {
  
  as.Date("2021-07-29")
  
}

#' Season 2122 start
season_2122_start <- function() {
  
  as.Date("2021-07-30")
  
}

#' Season 2122 end
season_2122_end <- function() {
  
  as.Date("2022-07-29")
  
}

#' Season 2223 start
season_2223_start <- function() {
  
  as.Date("2022-07-30")
  
}

#' Season 2223 end
season_2223_end <- function() {
  
  as.Date("2023-07-29")
  
}
