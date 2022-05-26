#' Calculate earnings grouped by stake
#' @param data tibble (see `read_and_prep_data`)
#' 
calculate_earnings_grouped_by_stake <- function(data) {
  
  data %>% 
    dplyr::filter(Stake > 0) %>% 
    dplyr::mutate(
      StakeMidpoint = kb.utils::cut_var(
        x = StakeMod, 
        breaks = breaks_stake
      )
    ) %>% 
    dplyr::group_by(StakeGroup, StakeMidpoint) %>% 
    calculate_earnings() %>% 
    dplyr::arrange(StakeMidpoint) %>% 
    dplyr::select(-StakeMidpoint)
  
}