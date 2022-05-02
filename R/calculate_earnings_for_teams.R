#' Calculate earnings for teams
#' @param data tibble (obtained from df_team_combined())
calculate_earnings_for_teams <- function(data) {
  
  data %>% 
    dplyr::filter(Bets > 0) %>% 
    dplyr::mutate(Correct = Accuracy * Bets / 100) %>% 
    dplyr::group_by(Team) %>% 
    dplyr::summarise(
      Bets = sum(Bets),
      Stake = round(sum(Stake), 0),
      Accuracy = round(100 * sum(Correct) / sum(Bets), 2),
      Revenue = round(sum(Revenue, na.rm = TRUE), 0), 
      Earnings = round(Revenue - Stake, 0), 
      Return = ifelse(Stake == 0, 0, round(100 * (Earnings / Stake), 1)), 
      .groups = "drop"
    ) %>% 
    dplyr::arrange(dplyr::desc(Bets))
  
}
  
