#' Start of this year
#' 
start_of_this_year <- function() {
  
  Sys.Date() %>% 
    substring(1, 4) %>% 
    paste0("-01-01") %>% 
    as.Date()
  
}