#' Color by
#' @param data DT::datatable object
#' @param var character string with column name to color
#' @param colors vector of colors to use
#' 
color_by <- function(data, var, colors) {
  
  df <- data$x$data
  
  breaks <- df %>% 
    dplyr::pull(var) %>% 
    quantile(probs = seq(from = 0, to = 1, by = 0.1))
  
  color_range <- grDevices::colorRampPalette(colors = colors)
  
  cuts <- c(-100, -75, -50, -25, -5, 0, 2.5, 5, 10, 20, 50, 75, 100)
  
  str_colors <- color_range(length(cuts) + 1)
  
  data %>% 
    DT::formatStyle(
      target = "row",
      columns = var, 
      backgroundColor = DT::styleInterval(
        cuts = cuts, 
        values = str_colors
      )
    )
  
}