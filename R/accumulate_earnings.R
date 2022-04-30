#' Accumulate earnings
#' @param data tibble with calculated earnings (see `calculate earnings`)
accumulate_earnings <- function(data) {
  
  data %>% 
    dplyr::arrange(BetDay) %>% 
    dplyr::summarise(Time = unique(BetDay), 
                     Bets = cumsum(Bets), 
                     Stake = cumsum(Stake), 
                     Revenue = cumsum(Revenue), 
                     Earnings = cumsum(Earnings), 
                     Return = round(100 * (Earnings / Stake), 2))
  
}