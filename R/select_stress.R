#' Select stress
#' @param data tibble
#' @param stress logical, TRUE means stressed columns are shown, FALSE means no stressing
#' 
select_stress <- function(data, stress) {
  
  if (stress) {
    
    var_chosen <- c("Bets*", "StakeLimit*", "NextLvl*", "MedianOdds*", "Stake*", "Accuracy*", 
                    "Revenue*", "Earnings*", "Return*", "ExpReturn*")
  } else {
    
    var_chosen <- c("Bets", "StakeLimit", "NextLvl", "MedianOdds", "Stake", "Accuracy", 
                    "Revenue", "Earnings", "Return", "ExpReturn")
    
  }
  
  if (names(data)[1] == "Bets") {
    var_common <- NULL ## "Bets" at locations 1 and 2 if not this fix
  } else {
    var_common <- names(data)[1] 
  }
  
  var_all <- c(var_common, var_chosen)
  
  data %>% 
    dplyr::select(dplyr::all_of(var_all)) %>% 
    dplyr::rename_with(~ gsub("\\*", "", .x))
  
}