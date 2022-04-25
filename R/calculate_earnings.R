#' Calculate earnings
#' @param data tibble with bets (see `read_data`)
#' 
calculate_earnings <- function(data) {
  
  data %>% 
    dplyr::summarise(Bets = dplyr::n(),
                     Stake = sum(Indsats, na.rm = TRUE),
                     Revenue = sum(Gevinst, na.rm = TRUE), 
                     Earnings = Revenue - Stake, 
                     Return = round(100 * (Earnings / Stake), 2)) %>% 
    dplyr::arrange(dplyr::desc(Bets))
  
}


