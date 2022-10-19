#' Simulate multiple earnings
#' @param data tibble (see read_and_prep_data)
#' @param x vector with accuracies used to simulate data with (numeric value between 0 and 1)
#' @param n number of simulations
simulate_multiple_earnings <- function(data, x, n) {
  
  m <- length(x)
  
  df_list <- list()
  
  for (j in 1:m) {
    
    df_list[[j]] <- simulate_earnings(data = data, x = x[j], n = n) %>% 
      dplyr::mutate(InputAccuracy = x[j])
    
  }
  
  df_out <- do.call("rbind", df_list)
  
  return(df_out)
  
}

#' Simulate earnings
#' @param data tibble (see read_and_prep_data)
#' @param x accuracy used to simulate data with (numeric value between 0 and 1)
#' @param n number of simulations
simulate_earnings <- function(data, x, n) {
  
  set.seed(1)
  
  df_list <- list()
  
  for (i in 1:n) {
    
    df_correct <- data %>% 
      dplyr::filter(Correct == 1)
    
    df_wrong <- data %>% 
      dplyr::filter(Correct == 0)
    
    n_bets        <- nrow(data)
    n_obs_correct <- nrow(df_correct)
    n_obs_wrong   <- nrow(df_wrong)
    n_sim_correct <- round(n_bets * x)
    n_sim_wrong   <- round(n_bets * (1 - x))
    
    id_correct <- sample(x = 1:n_obs_correct, size = n_sim_correct, replace = TRUE)
    id_wrong   <- sample(x = 1:n_obs_wrong, size = n_sim_wrong, replace = TRUE)
    
    df_sim_correct <- df_correct %>% 
      dplyr::slice(id_correct)
    
    df_sim_wrong <- df_wrong %>% 
      dplyr::slice(id_wrong)
    
    df_sim <- dplyr::bind_rows(
      df_sim_correct,
      df_sim_wrong
    )
    
    df_list[[i]] <- df_sim %>% 
      calculate_earnings() %>% 
      dplyr::mutate(id = i)
    
    cat("\r", i, "of", n)
    flush.console()
    
  }
  
  df_out <- do.call("rbind", df_list)
  
  return(df_out)
  
}