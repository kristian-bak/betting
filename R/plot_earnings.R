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
#' @param y character string with column name to plot on y axis
plot_return <- function(data, x, y) {
  
  data %>% 
    dplyr::mutate(Win = dplyr::if_else(Return >= 0, "Win", "Loss")) %>% 
    plotly::plot_ly(source = "return") %>% 
    plotly::add_bars(x = ~get(x), y = ~get(y), color = ~Win, colors = c("#FFC3B8", "#C3F5C3")) %>% 
    plotly::layout(showlegend = FALSE, 
                   xaxis = list(title = x), 
                   yaxis = list(title = y),
                   title = paste0(x, "ly", " ", y))
  
}