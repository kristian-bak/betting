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
                     .groups = "drop") %>% 
    add_stake_limit()
  
  str_joining_var <- names(df_earnings)[1] ## GameType if grouped by GameType
  
  if (str_joining_var == "Bets") { ## No grouping (i.e. calculate earnings on all data)
    var_rename <- c("Stake", "StakeLimit", "NextLvl", "MedianOdds", "Accuracy", "Revenue", "Earnings", "Return", "ExpReturn")
  } else {
    var_rename <- c("Bets", "StakeLimit", "NextLvl", "MedianOdds", "Stake", "Accuracy", "Revenue", "Earnings", "Return", "ExpReturn")
  }
  
  ## Earnings from freebets are removed in stressed calculations and top 1 % earnings are removed as well
  df_earnings_stress <- data_tmp %>% 
    dplyr::mutate(Revenue = dplyr::if_else(Correct == 1 & !is.na(Freebet), Stake * Odds, Revenue),
                  Earnings = round(Revenue - Stake, 0), 
                  EarningsQ99 = quantile(Earnings, probs = 0.99, na.rm = TRUE)) %>%
    dplyr::filter(Earnings < EarningsQ99) %>% 
    dplyr::summarise(Bets = if (is.null(var_bets)) sum(Exposure) else sum(get(var_bets)),
                     MedianOdds = median(Odds),
                     Stake = round(sum(get(var_stake), na.rm = TRUE), 0),
                     Accuracy = round(100 * sum(Correct * Exposure) / sum(Exposure), 2),
                     Revenue = round(sum(get(var_revenue), na.rm = TRUE), 0), 
                     Earnings = round(Revenue - Stake, 0), 
                     Return = ifelse(Stake == 0, 0, round(100 * (Earnings / Stake), 1)),
                     ExpReturn = return_in_pct(x = MedianOdds * (Accuracy / 100)),
                     .groups = "drop") %>% 
    add_stake_limit() %>% 
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
    
    ## Ensure only unique columns are added to the join
    stress_names <- names(df_earnings_stress) 
    
    unique_names <- stress_names[!stress_names %in% names(df_earnings)]
    
    unique_vars <- c(unique_names, str_joining_var)
    
    df_out <- df_earnings %>% 
      dplyr::full_join(df_earnings_stress %>% dplyr::select(unique_vars), by = str_joining_var)
  }
  
  return(df_out)
    
}


