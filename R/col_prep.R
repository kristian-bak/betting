#' Column prep
#' @param data tibble (see `read_data`)
#' @param breaks breaks used to cut odds column
#' 
col_prep <- function(data, breaks) {
  
  if (missing(breaks)) {
    breaks <- c(
      seq(from = 1, to = 2, by = 0.25), 
      seq(from = 2.5, to = 15, by = 2)
    )
  }
  
  breaks_stake <- c(0, 25, 50, 75, 100, 200)
  
  data_tmp <- data %>% 
    dplyr::mutate(HomeTeam   = map_game_to_home_team(Match), 
                  AwayTeam   = map_game_to_away_team(Match), 
                  OddsMod    = dplyr::if_else(Odds > 5, 5, Odds),
                  OddsGroup  = cut(OddsMod, breaks = breaks, include.lowest = TRUE) %>% as.character(),
                  BetDay     = as.Date(BetDay),
                  MatchDay   = as.Date(MatchDay), 
                  StakeMod   = dplyr::if_else(Stake > 200, 200, Stake),
                  StakeGroup = cut(Stake, breaks = breaks_stake, include.lowest = TRUE) %>% as.character())
  
  df_count_bets <- data_tmp %>% 
    dplyr::mutate(ExposureBets = dplyr::if_else(is.na(Revenue) | is.na(Match), 0, 1)) %>% 
    dplyr::group_by(MatchDay, Match) %>% 
    dplyr::summarise(Bets = sum(ExposureBets), .groups = "drop")
  
  data_out <- data_tmp %>% 
    dplyr::left_join(df_count_bets, by = c("MatchDay", "Match"))
  
  return(data_out)
  
}