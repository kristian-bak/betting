#' Plotly double y axis
#' @param data data.frame
#' @param type type of plot (for instance scatter)
#' @param mode plotly mode (for instance lines)
#' @param x character string with column to plot on the x axis
#' @param yaxis1 character string with column to plot on the y axis 1 (i.e. on the left hand side)
#' @param yaxis2 character string with column to plot on the y axis 2 (i.e. on the right hand side)
#' @param legend1 legend for yaxis1
#' @param legend2 legend for yaxis2
#' @param xaxis_title title on the x axis
#' @param yaxis1_title title on y axis 1
#' @param yaxis2_title title on y axis 2
#' @param main_title title on top of the plot
#' 
plotly_double_yaxis <- function(data, type, mode, x, yaxis1, yaxis2, legend1, legend2, xaxis_title, yaxis1_title, yaxis2_title, main_title) {
  
  fig <- plotly::plot_ly(data = data, x = ~get(x), y = ~get(yaxis1), name = legend1, mode = mode, type = type) %>% 
    plotly::layout(title = main_title, 
                   xaxis = list(title = xaxis_title),
                   yaxis = list(title = yaxis1_title))
  
  if (!missing(yaxis2)) {
    
    if (yaxis2 != "") {
      
      ay <- list(
        overlaying = "y",
        side = "right",
        title = yaxis2_title
      )
      
      fig <- fig %>% 
        plotly::add_trace(y = ~get(yaxis2), name = legend2, yaxis = "y2", mode = mode, type = type) %>% 
        plotly::layout(yaxis2 = ay) 
      
    }
    
  }
  
  return(fig)
  
}
