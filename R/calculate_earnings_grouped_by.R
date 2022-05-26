#' Calculate earnings grouped by
#' @param data tibble (see `read_and_prep_data`)
#' @param x variable name (tidy approach, i.e. not strings)
#' 
calculate_earnings_grouped_by <- function(data, x) {
  
  data %>% 
    dplyr::group_by({{x}}) %>% 
    calculate_earnings() %>% 
    dplyr::arrange(dplyr::desc(Bets))
  
}