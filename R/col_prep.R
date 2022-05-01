#' Column prep
#' @param data tibble (see `read_data`)
#' @param breaks breaks used to cut odds column
#' 
col_prep <- function(data, breaks) {
  
  data %>% 
    dplyr::mutate(HomeTeam  = map_game_to_home_team(Match), 
                  AwayTeam  = map_game_to_away_team(Match), 
                  OddsMod   = dplyr::if_else(Odds > 5, 5, Odds),
                  OddsGroup = cut(OddsMod, breaks = breaks) %>% as.character(),
                  BetDay    = as.Date(BetDay),
                  MatchDay  = as.Date(MatchDay))
  
}