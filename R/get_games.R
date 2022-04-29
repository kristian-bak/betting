#' Get games
#' @param data tibble from `read_data`
#' 
get_games <- function(data) {
  
  data %>% 
    dplyr::pull(Spil) %>% 
    unique() %>% 
    sort()
  
}