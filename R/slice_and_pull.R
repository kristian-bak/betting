#' Slice and pull
#' @param data tibble
#' @param i integer specifying which row(s) to slice
#' @param j variable name (tidy approach) specifying which column to pull
#' 
slice_and_pull <- function(data, i, j) {
  
  data %>% 
    dplyr::slice(i) %>% 
    dplyr::pull({{j}})
  
}