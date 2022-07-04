test_that("Test: calculate_earnings", {
  
  data <- read_data()

  out <- data %>% 
    calculate_earnings()
  
  expect_equal(class(out)[1], "tbl_df")
  
  expect_equal(names(out),  c("Bets", "MedianOdds", "Stake", "Accuracy", "Revenue", "Earnings", 
                              "Return", "ExpReturn", 
                              "Bets*", "Stake*", "Accuracy*", "Revenue*", "Earnings*", "Return*"))
  
  expect_true(sapply(out, is.numeric) %>% 
    all()
  )
    
})
