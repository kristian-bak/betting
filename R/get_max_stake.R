#' Get max stake
#' @param data tibble obtained from `read_and_prep_data`
#' 
get_max_stake <- function(data) {
  
  data %>% 
    dplyr::pull(Stake) %>% 
    max() %>% 
    ceiling()
  
}