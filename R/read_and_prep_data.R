#' Read and prep data
#' @param breaks breaks used to cut odds column (default is 12-dim sequence from 1 to 14.5)
#' 
read_and_prep_data <- function(breaks) {
  
  if (missing(breaks)) {
    breaks <- c(1, 1.25, 1.50, 1.75, 2, 2.5, 3, 4, 5)
  }
  
  read_data() %>% 
    col_prep(breaks = breaks)
  
}