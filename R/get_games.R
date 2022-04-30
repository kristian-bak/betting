#' Get games
#' @param data tibble from `read_data`
#' 
get_games <- function(data) {
  
  data %>% 
    dplyr::pull(Game) %>% 
    unique() %>% 
    sort()
  
}