#' Add zero betting weeks
#' @param data tibble from calculate_earnings
#' 
add_zero_betting_weeks <- function(data) {
  
  max_week_number <- data %>% 
    dplyr::mutate(week_number = kb.utils::remove_everything_before("-", Week) %>% as.numeric()) %>% 
    dplyr::pull(week_number) %>% 
    max()
  
  year <- data %>% 
    dplyr::mutate(year = kb.utils::remove_everything_after("-", Week) %>% as.numeric()) %>% 
    dplyr::pull(year) %>% 
    unique()
  
  df_time_period <- tidyr::expand_grid(year = year, week_number = add_zero(1:max_week_number)) %>% 
    dplyr::mutate(Week = paste(year, week_number, sep = "-")) %>% 
    dplyr::select(Week)
  
  data %>% 
    dplyr::full_join(df_time_period, by = "Week") %>% 
    replace(is.na(.), 0)
  
}