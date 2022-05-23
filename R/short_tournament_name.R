#' Short tournament name
#' @param x character vector with tournamnet names
#' @return character vector with abbreviated tournament names
#' 
short_tournament_name <- function(x) {
  
  x[x == "Premier League"]   <- "PL"
  x[x == "Champions League"] <- "CL"
  
  return(x)
  
}