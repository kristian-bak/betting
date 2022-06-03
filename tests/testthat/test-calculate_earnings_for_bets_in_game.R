test_that("Test: calculate_earnings_for_bets_in_game", {
  
  out <- read_and_prep_data() %>% 
    dplyr::filter(BetDay >= "2022-01-01" & BetDay <= "2022-04-01") %>% 
    calculate_earnings_for_bets_in_game()
  
  expect_equal(names(out), c("BetsInGame", "Bets", "Stake", "Accuracy", "Revenue", "Earnings", "Return"))
  
  expect_equal(out %>% dplyr::pull(BetsInGame), 1:3)
  
  expect_equal(out %>% dplyr::pull(Bets), c(9, 2, 3))
  
  expect_equal(out %>% dplyr::pull(Accuracy), c(80, 50, 70))
  
  expect_equal(out %>% dplyr::pull(Return), c(21, -53, 28.999999999999996))
  
})
