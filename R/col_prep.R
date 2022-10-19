#' Column prep
#' @param data tibble (see `read_data`)
#' @param breaks_odds breaks used to cut odds column
#' @param breaks_stake breaks used to cut stake column
#' @param bound_odds integer specifying when to bound the odds column (all values above the bound will take the value `bound_odds`)
#' @param bound_stake integer specifying when to bound the stake column (all values above the bound will take the value `bound_stake`)
col_prep <- function(data, breaks_odds, breaks_stake, bound_odds, bound_stake) {
  
  data_tmp <- data %>% 
    dplyr::mutate(HomeTeam   = map_game_to_home_team(Match), 
                  AwayTeam   = map_game_to_away_team(Match), 
                  OddsMod    = dplyr::if_else(Odds >= bound_odds, bound_odds, Odds),
                  OddsGroup  = cut(OddsMod, breaks = breaks_odds, include.lowest = TRUE) %>% as.character(),
                  BetDay     = as.Date(BetDay),
                  MatchDay   = as.Date(MatchDay), 
                  StakeMod   = dplyr::if_else(Stake > bound_stake, bound_stake, Stake),
                  StakeGroup = cut(StakeMod, breaks = breaks_stake, include.lowest = TRUE) %>% as.character(), 
                  ID         = dplyr::row_number() %>% rev(), 
                  DaysDiff   = difftime(MatchDay, BetDay, units = "d") %>% as.numeric(), 
                  Bookmaker  = dplyr::if_else(is.na(Bookmaker), "Danske Spil", Bookmaker), 
                  Year = substring(MatchDay, 1, 4),
                  Month = substring(MatchDay, 1, 7), 
                  Week = paste0(Year, "-", add_zero(lubridate::isoweek(MatchDay))), 
                  PredWin = dplyr::case_when(
                    GameType == "Dobbeltchance" & grepl("Uafgjort eller", Bet) ~ "Away",
                    GameType == "Dobbeltchance" & !grepl("Uafgjort eller", Bet) ~ "Home",
                    GameType == "Kampvinder" & Bet == "1" ~ "Home",
                    GameType == "Kampvinder" & Bet == "X" ~ "Draw",
                    GameType == "Kampvinder" & Bet == "2" ~ "Away",
                    GameType == "Kampvinder" & kb.utils::remove_everything_before(pattern = ": ", x = Bet) == HomeTeam ~ "Home",
                    GameType == "Kampvinder" & kb.utils::remove_everything_before(pattern = ": ", x = Bet) == AwayTeam ~ "Away",
                    TRUE ~ "Not directed bet"
                  )
    )
  
  ## get_selected_subset must be updated if adding a new column here
  
  df_count_bets <- data_tmp %>% 
    dplyr::mutate(ExposureBets = dplyr::if_else(is.na(Revenue) | is.na(Match), 0, 1)) %>% 
    dplyr::group_by(MatchDay, Match) %>% 
    dplyr::summarise(Bets = sum(ExposureBets), .groups = "drop")
  
  data_out <- data_tmp %>% 
    dplyr::left_join(df_count_bets, by = c("MatchDay", "Match"))
  
  return(data_out)
  
}