#' Map game to home team
#' @param x character string with home team - away team
#' @return character string with home team
map_game_to_home_team <- function(x) {
  
  kb.utils::remove_everything_after(pattern = "-", x = x) %>% trimws()
  
}

#' Map game to away team
#' @param x character string with home team - away team
#' @return character string with away team
map_game_to_away_team <- function(x) {
  
  kb.utils::remove_everything_before(pattern = "-", x = x) %>% trimws()
  
}