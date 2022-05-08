#' Floor to nearest half
#' @param x numeric vector
#' 
floor_to_nearest_half <- function(x) {
  
  floor(x / 0.5) * 0.5
  
}

#' Calculate smart stake
#' @param odds odds as numeric value
#' @param stake stake as a numeric value
#' @param eps epsilon, a numerid value, used fine smart stakes around the stake value (default is 0.5)
#' 
calculate_smart_stake <- function(odds, stake, eps = 0.5) {
  
  stake_range <- seq(from = stake - eps, to = stake + eps, by = 0.01)
  
  revenue <- floor_to_nearest_half(odds * stake_range)
  
  dplyr::tibble(stake_range, stake, revenue, odds) %>% 
    dplyr::group_by(revenue) %>% 
    dplyr::summarise(
      stake_orignal = unique(stake),
      odds = unique(odds), 
      revenue_original = floor_to_nearest_half(odds * stake_orignal),
      stake_smart = min(stake_range),
    ) %>% 
    dplyr::select(odds, stake_orignal, revenue_original, stake_smart, revenue) %>% 
    dplyr::rename(revenue_smart = revenue) %>% 
    dplyr::filter(revenue_smart >= revenue_original) %>% 
    dplyr::mutate(earnings_original = revenue_original - stake_orignal, 
                  earnings_smart = revenue_smart - stake_smart, 
                  return_original = 100 * round(earnings_original / stake_orignal, 2), 
                  return_smart = 100 * round(earnings_smart / stake_smart, 2), 
                  excess_return = return_smart - return_original)
  
}
