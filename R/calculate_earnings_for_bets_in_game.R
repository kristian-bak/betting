#' Calculate earnings for bets in game
#' @param data tibble with bets (see `read_data`)
#' 
calculate_earnings_for_bets_in_game <- function(data) {
  
  data %>% 
    dplyr::filter(!is.na(MatchDay) & !is.na(Match)) %>% 
    dplyr::group_by(MatchDay, Match) %>% 
    calculate_earnings() %>% 
    dplyr::mutate(BetsInGame = Bets, 
                  Correct = Accuracy * Bets / 100) %>% 
    dplyr::filter(BetsInGame > 0) %>% ## doubles, triples, and n-lings
    dplyr::group_by(BetsInGame) %>% 
    dplyr::summarise(Bets = sum(Bets),
                     Stake = sum(Stake),
                     Accuracy = 100 * round(sum(Correct) / sum(Bets), 1), 
                     Revenue = round(sum(Revenue), 1), 
                     Earnings = round(sum(Earnings), 1), 
                     Return = 100 * round(Earnings / Stake, 2))
  
}