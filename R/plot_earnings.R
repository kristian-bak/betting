#' Plot earnings
#' @param data data.frame
#' @param y1 character string with name of y
#' @param y2 optional character string with name of second y axis
#' 
plot_earnings <- function(data, y1, y2) {
  
  data %>% 
    plotly_double_yaxis(
      type = "scatter", 
      mode = "lines", 
      x = "Time", 
      yaxis1 = y1, 
      yaxis2 = y2,
      legend1 = y1,
      legend2 = y2,
      xaxis_title = "Time", 
      yaxis1_title = y1, 
      yaxis2_title = y2,
      main_title = map_yaxis_to_plot_title(y = y1)
    )
  
}