#' Plot distribution using histogram
#' @param data tibble obtained from `read_and_prep_data`
#' @param var character string with column name to select (any from data)
#' 
plot_distribution <- function(data, var) {
  
  if (var == "Odds") {
    
    q99 <- quantile(data$Odds, probs = 0.99)
    
    plot_data <- data %>% 
      dplyr::filter(Odds < q99)
    
  } else {
    
    plot_data <- data
    
  }
  
  plot_data %>% 
    plotly::plot_ly(x = ~get(var), type = "histogram") %>% 
    plotly::layout(xaxis = list(title = var), 
                   yaxis = list(title = "Counts"),
                   title = "Histogram of covariate")
  
}