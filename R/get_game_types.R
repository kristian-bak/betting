#' Get game types
#' @param data tibble from `read_data`
#' 
get_game_types <- function(data) {
  
  data %>% 
    dplyr::pull(GameType) %>% 
    unique() %>% 
    sort()
  
}