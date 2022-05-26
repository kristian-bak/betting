#' Calculate earnings grouped by team
#' @param data tibble (see `read_and_prep_data`)
#' @param data variable name (tidy approach)
#' 
calculate_earnings_grouped_by_team <- function(data, x) {
  
  data %>% 
    dplyr::filter(!is.na({{x}})) %>% 
    dplyr::group_by({{x}}) %>% 
    calculate_earnings() %>% 
    dplyr::arrange(dplyr::desc(Bets)) %>% 
    dplyr::rename(Team = {{x}})
  
}