#' Get team names
#' @param data tibble with historical bets (see `read_data`) having columns HomeTeam and AwayTeam (see map_game_to_*)
#' 
get_team_names <- function(data) {
  
  c(data %>% dplyr::pull(HomeTeam), 
    data %>% dplyr::pull(AwayTeam)) %>% 
    na.omit() %>% 
    unique() %>% 
    sort()
  
}