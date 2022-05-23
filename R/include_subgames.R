#' Get subset and subgames realted to game type filtering
#' @param data tibble obtained from `read_and_prep_data`
#' @param ... filter options
#' @return subset of data with GameType == game_type and the associated doubles and triples included
get_subset_and_subgames <- function(data, ...) {
  
  df1 <- data %>% 
    dplyr::filter(...)
  
  sub_bets <- df1 %>% 
    dplyr::pull(Bet) %>% 
    paste0(collapse = "|")
  
  df2 <- data %>% 
    dplyr::filter(grepl(sub_bets, Bet))
  
  return(df2)
  
}