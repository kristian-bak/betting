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

#' Plot return
#' @param data tibble with earnings
#' @param x character string with column name to plot on x axis
#' @param y1 character string with column name to plot on y axis 1
#' @param y2 character string with column name to plot on y axis 2
#' @param source source paramater passed on to plotly::plot_ly()
plot_return <- function(data, x, y1, y2, source) {
  
  #data %>% 
  #  dplyr::mutate(Win = dplyr::if_else(Return >= 0, "Win", "Loss")) %>% 
  #  plotly::plot_ly(source = "return") %>% 
  #  plotly::add_bars(x = ~get(x), y = ~get(y), color = ~Win, colors = c("#FFC3B8", "#C3F5C3")) %>% 
  #  plotly::layout(showlegend = FALSE, 
  #                 xaxis = list(title = x), 
  #                 yaxis = list(title = y),
  #                 title = paste0(x, "ly", " ", y))
  #
  
  data %>% 
    plotly_double_yaxis(
      type = "scatter", 
      mode = "lines", 
      x = x, 
      yaxis1 = y1, 
      yaxis2 = y2,
      legend1 = y1,
      legend2 = y2,
      xaxis_title = x, 
      yaxis1_title = y1, 
      yaxis2_title = y2,
      main_title = paste0(x, "ly", " ", paste0(y1, y2, collapse = " and ")),
      source = source
    )
  
}