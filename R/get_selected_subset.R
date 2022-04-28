#' Get selected subset
#' @param data tibble obtained from `read_data`
#' @param ... filter options
#' 
get_selected_subset <- function(data, ...) {
  
  data %>% 
    dplyr::filter(...) %>% 
    dplyr::select(-Købsdato, -HomeTeam, -AwayTeam)
  
}