#' Get accuracy confidence interval
#' @param data tibble with bets (see read_and_prep_data)
get_accuracy_ci <- function(data) {
  
  df_earnings <- data %>% 
    calculate_earnings()
  
  obs_acc   <- df_earnings$Accuracy / 100
  n_bets    <- df_earnings$Bets
  n_correct <- n_bets * obs_acc
  
  df_test <- prop.test(x = n_correct, n = n_bets, conf.level = 0.95)
  
  ci <- df_test$conf.int %>% 
    as.numeric()
  
  out <- c(ci[1], obs_acc, ci[2])
  
  return(out)
  
}