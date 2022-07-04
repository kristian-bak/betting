#' Calculate earnings
#' @param data tibble with bets (see `read_data`)
#' @param var_bets character string with variable name for "bets" column (Default is NULL, meaning summing exposure)
#' @param var_stake character string with variable name for "stake" column (Default is "Stake")
#' @param var_revenue character string with variable name for "Revenue" column (Default is "Revenue")
#' 
calculate_earnings <- function(data, var_bets = NULL, var_stake = "Stake", var_revenue = "Revenue") {
  
  data_tmp <- data %>% 
    dplyr::mutate(Exposure = if (!is.null(var_bets)) get(var_bets) else dplyr::if_else(Stake > 0, 1, 0), 
                  Correct = if ("Correct" %in% names(data)) Correct else Accuracy * Bets / 100)
  
  df_earnings <- data_tmp %>% 
    dplyr::summarise(Bets = if (is.null(var_bets)) sum(Exposure) else sum(get(var_bets)),
                     MedianOdds = median(Odds),
                     Stake = round(sum(get(var_stake), na.rm = TRUE), 0),
                     Accuracy = round(100 * sum(Correct * Exposure) / sum(Exposure), 2),
                     Revenue = round(sum(get(var_revenue), na.rm = TRUE), 0), 
                     Earnings = round(Revenue - Stake, 0), 
                     Return = ifelse(Stake == 0, 0, round(100 * (Earnings / Stake), 1)), 
                     ExpReturn = return_in_pct(x = MedianOdds * (Accuracy / 100)),
                     .groups = "drop")
  
  str_joining_var <- names(df_earnings)[1]
  
  if (str_joining_var == "Bets") {
    var_rename <- c("Stake", "Accuracy", "Revenue", "Earnings", "Return")
  } else {
    var_rename <- c("Bets", "Stake", "Accuracy", "Revenue", "Earnings", "Return")
  }
  
  ## Earnings from freebets are removed in stressed calculations and top 1 % earnings are removed as well
  df_earnings_stress <- data_tmp %>% 
    dplyr::mutate(Revenue = dplyr::if_else(Correct == 1 & !is.na(Freebet), Stake * Odds, Revenue),
                  Earnings = round(Revenue - Stake, 0), 
                  EarningsQ99 = quantile(Earnings, probs = 0.99, na.rm = TRUE)) %>%
    dplyr::filter(Earnings < EarningsQ99) %>% 
    dplyr::summarise(Bets = if (is.null(var_bets)) sum(Exposure) else sum(get(var_bets)),
                     Stake = round(sum(get(var_stake), na.rm = TRUE), 0),
                     Accuracy = round(100 * sum(Correct * Exposure) / sum(Exposure), 2),
                     Revenue = round(sum(get(var_revenue), na.rm = TRUE), 0), 
                     Earnings = round(Revenue - Stake, 0), 
                     Return = ifelse(Stake == 0, 0, round(100 * (Earnings / Stake), 1)), 
                     .groups = "drop") %>% 
    dplyr::rename_with(.cols = dplyr::all_of(var_rename), 
                       .fn = ~paste0(.x, "*"))
  
  ## If str_joining_var == "Bets", then no grouping has been applied, meaning both
  ## df_earnings and df_earnings_stress have only 1 row
  if (str_joining_var == "Bets") {
    df_out <- dplyr::bind_cols(
      df_earnings,
      df_earnings_stress %>% dplyr::rename(`Bets*` = Bets)
    )
  } else {
    df_out <- df_earnings %>% 
      dplyr::full_join(df_earnings_stress, by = str_joining_var)
  }
  

  
  return(df_out)
    
}


