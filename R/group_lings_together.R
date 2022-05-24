#' Group lings together
#' @param x character vector with game types (for instance column GameType from `read_and_prep_data`)
#' @param group logical (TRUE/FALSE) indicating whether or not grouping should take place or not. 
#' If FALSE, x is returned
#' @return character vector with game types where all lings have been modified to "X-ling"
#' 
group_lings_together <- function(x, group) {
  
  if (!group) {
    return(x)
  }
  
  x[grepl(pattern = "-ling", x = x)] <- "X-ling"
  
  return(x)
  
}

#' Group double and triple as lings
#' @param x character vector with game types (for instance column GameType from `read_and_prep_data`)
#' @param group logical (TRUE/FALSE) indicating whether or not grouping should take place or not. 
group_double_and_triple_as_lings <- function(x, group) {
  
  if (!group) {
    return(x)
  }
  
  x[grepl(pattern = "Double|Trippel", x = x)] <- "X-ling"
  
  return(x)
  
}