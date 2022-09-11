#' Add stake limit
#' @param data tibble
add_stake_limit <- function(data) {
  
  data %>% 
    dplyr::mutate(BetsGroup = cut(Bets, breaks = c(0, 10, 20, 50, 75, 100, 125, 150, 175, 200, 10000), 
                                  include.lowest = TRUE)) %>% 
    dplyr::mutate(StakeLimit = dplyr::case_when(
      BetsGroup == "[0,10]" ~ 25,    
      BetsGroup == "(10,20]" & Return >= 0 ~ 50,
      BetsGroup == "(10,20]" & Return < 0 & Return >= -20 ~ 25,
      BetsGroup == "(10,20]" & Return < -20 ~ 10,
      BetsGroup == "(20,50]" & Return >= 0 ~ 75,
      BetsGroup == "(20,50]" & Return < 0 & Return >= -20 ~ 10,
      BetsGroup == "(20,50]" & Return < -20 ~ 0,
      BetsGroup == "(50,75]" & Return >= 0 ~ 100,
      BetsGroup == "(75,100]" & Return >= 0 ~ 150,
      BetsGroup == "(100,125]" & Return >= 0 ~ 200,
      BetsGroup == "(125,150]" & Return >= 0 ~ 300,
      BetsGroup == "(150,175]" & Return >= 0 ~ 400,
      BetsGroup == "(175,200]" & Return >= 0 ~ 500,
      BetsGroup == "(200,10000]" & Return >= 0 ~ 600,
      Return < 0 ~ 0
    ), 
    Upper = kb.utils::remove_everything_before(pattern = ",", x = BetsGroup) %>% 
      gsub("]", "", .) %>% 
      as.numeric(), 
    NextLvl = dplyr::if_else(StakeLimit == 0, as.numeric(NA), Upper - Bets)) %>% 
    dplyr::select(-Upper, -BetsGroup) %>% 
    dplyr::relocate(StakeLimit, .before = MedianOdds) %>% 
    dplyr::relocate(NextLvl, .before = MedianOdds)
  
}