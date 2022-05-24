#' Calculate earnings
#' @param data tibble with bets (see `read_data`)
#' @param var_bets character string with variable name for "bets" column (Default is NULL, meaning summing exposure)
#' @param var_stake character string with variable name for "stake" column (Default is "Stake")
#' @param var_revenue character string with variable name for "Revenue" column (Default is "Revenue")
#' 
calculate_earnings <- function(data, var_bets = NULL, var_stake = "Stake", var_revenue = "Revenue") {
  
  data %>% 
    dplyr::mutate(Exposure = if (!is.null(var_bets)) get(var_bets) else dplyr::if_else(Stake > 0, 1, 0), 
                  Correct = if ("Correct" %in% names(data)) Correct else Accuracy * Bets / 100) %>% 
    dplyr::summarise(Bets = if (is.null(var_bets)) sum(Exposure) else sum(get(var_bets)),
                     Stake = round(sum(get(var_stake), na.rm = TRUE), 0),
                     Accuracy = round(100 * sum(Correct * Exposure) / sum(Exposure), 2),
                     Revenue = round(sum(get(var_revenue), na.rm = TRUE), 0), 
                     Earnings = round(Revenue - Stake, 0), 
                     Return = ifelse(Stake == 0, 0, round(100 * (Earnings / Stake), 1)), 
                     .groups = "drop")
}


