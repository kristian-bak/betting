#' Read and prep data
#' @inheritParams col_prep
#' 
read_and_prep_data <- function(
  breaks_odds = c(1, 1.25, 1.50, 1.75, 2, 2.5, 3, 4, 5), 
  breaks_stake = c(0, 25, 50, 75, 100, 200) , 
  bound_odds = 5, 
  bound_stake = 200) {
  
  read_data() %>% 
    col_prep(
      breaks_odds = breaks_odds, 
      breaks_stake = breaks_stake, 
      bound_odds = bound_odds, 
      bound_stake = bound_stake
    )
  
}