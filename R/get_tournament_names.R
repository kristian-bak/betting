#' Get tournament names
#' @param data tibble from `read_data`
#' 
get_tournament_names <- function(data) {
  
  data %>% 
    dplyr::pull(Turnering) %>% 
    unique() %>% 
    sort()
  
}