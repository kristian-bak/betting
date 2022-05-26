#' Get selectize choices
#' @param data tibble
#' @param x name of variable to pull (must be a column in data)
#' 
get_selectize_choices <- function(data, x) {
  
  data %>% 
    dplyr::pull({{x}}) %>% 
    unique() %>% 
    sort()
  
}