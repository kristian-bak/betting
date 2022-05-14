test_that("Test: cumsum_with_reset", {
  
  out1 <- cumsum_with_reset(x = c(1:5, 0, 6:10))
  out2 <- cumsum_with_reset(x = c(1, 1, 0, 1, 1))
  
  expect_equal(out1, c(1, 3, 6, 10, 15, 0, 6, 13, 21, 30, 40))
  
  expect_equal(out2, c(1, 2, 0, 1, 2))
  
})
