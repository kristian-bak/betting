#' Plot earnings
#' 
plot_earnings <- function(data, y) {
  
  data %>% 
    plotly::plot_ly(type = "scatter", x = ~Time, y = ~get(y), mode = "lines") %>% 
    plotly::layout(yaxis = list(title = y))
  
}