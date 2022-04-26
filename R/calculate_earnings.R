#' Calculate earnings
#' @param data tibble with bets (see `read_data`)
calculate_earnings <- function(data, order_by = NULL) {
  
  data %>% 
    dplyr::summarise(Bets = dplyr::n(),
                     Stake = round(sum(Indsats, na.rm = TRUE), 0),
                     Revenue = round(sum(Gevinst, na.rm = TRUE), 0), 
                     Earnings = round(Revenue - Stake, 0), 
                     Return = round(100 * (Earnings / Stake), 1))
}


