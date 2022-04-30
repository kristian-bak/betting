#' Calculate earnings
#' @param data tibble with bets (see `read_data`)
calculate_earnings <- function(data, var_bets = NULL, var_stake = "Stake", var_revenue = "Revenue", order_by = NULL) {
  
  data %>% 
    dplyr::mutate(Exposure = if (!is.null(var_bets)) NA else dplyr::if_else(is.na(Revenue), 0, 1)) %>% 
    dplyr::summarise(Bets = if (is.null(var_bets)) sum(Exposure) else sum(get(var_bets)),
                     Stake = round(sum(get(var_stake), na.rm = TRUE), 0),
                     Revenue = round(sum(get(var_revenue), na.rm = TRUE), 0), 
                     Earnings = round(Revenue - Stake, 0), 
                     Return = ifelse(Stake == 0, 0, round(100 * (Earnings / Stake), 1)))
}


