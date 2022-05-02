test_that("Test: plotly_double_yaxis", {
  
  data <- read_and_prep_data()
  
  out1 <- kb.utils::catch_error(
    plotly_double_y_axis(
      data = data, 
      type = "scatter", 
      mode = "lines", 
      x = "BetDay", 
      yaxis1 = "Stake", 
      yaxis2 = "Revenue", 
      legend1 = "Stake", 
      legend2 = "Revenue", 
      xaxis_title = "Bet day title", 
      yaxis1_title = "Stake title", 
      yaxis2_title = "Revenue title", 
      main_title = "Double plot"
    )
  )
  
  expect_true(is.null(out1$error))
  
  out2 <- kb.utils::catch_error(
    plotly_double_y_axis(
      data = data, 
      type = "scatter", 
      mode = "lines", 
      x = "BetDay", 
      yaxis1 = "Stake", 
      legend1 = "Stake", 
      xaxis_title = "Bet day title", 
      yaxis1_title = "Stake title", 
      main_title = "Double plot"
    )
  )
  
  expect_true(is.null(out2$error))
  
})
