#' Get selected subset
#' @param data tibble obtained from `read_data`
#' @param ... filter options
#' @param var character string with variable name used to select subset based on. If "GameType", 
#' then `get_subset_and_subgames` is called to ensure double and tripple subgames are included as well
#' Default is "", meaning subgames won't be included
#' @param value character string with value of GameType column (only relevant if "Double", "Trippel" or any ling)
#' @return subset of data
#' 
get_selected_subset <- function(data, ..., var = "", value = "") {
  
  if (var == "GameType" & (value == "Double" | value == "Trippel" | grepl(pattern = "-ling", x = value))) {
    df_tmp <- get_subset_and_subgames(data = data, ...)
  } else {
    df_tmp <- data %>% 
      dplyr::filter(...)
  }
  
  df_tmp %>% 
    dplyr::select(-BetDay, -HomeTeam, -AwayTeam, -OddsGroup, 
                  -OddsMod, -StakeGroup, -StakeMod, -ID, -GameType, 
                  -Country, -Freebet, -Bookmaker) %>% 
    dplyr::rename(Tourn = Tournament) %>% 
    dplyr::mutate(Tourn = short_tournament_name(Tourn))
  
}