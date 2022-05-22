#' Arrange by odds group
#' @param data tibble obtained from `read_and_prep_data`
#' @return tibble with rows rearranged according to odds group
#' 
arrange_by_odds_group <- function(data) {
  
  data %>% 
    dplyr::mutate(OddsGroup1 = kb.utils::remove_everything_after(pattern = ",", x = OddsGroup) %>% 
                  substring(text = ., first = 2, last = 5) %>% 
                  as.numeric()) %>% 
    dplyr::arrange(OddsGroup1) %>% 
    dplyr::select(-OddsGroup1)
  
}