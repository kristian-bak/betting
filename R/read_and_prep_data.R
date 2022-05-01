#' Read and prep data
#' @param breaks breaks used to cut odds column (default is 12-dim sequence from 1 to 14.5)
#' 
read_and_prep_data <- function(breaks) {
  
  if (missing(breaks)) {
    breaks <- c(
      seq(from = 1, to = 2, by = 0.25), 
      seq(from = 2.5, to = 15, by = 2)
    )
  }
  
  read_data() %>% 
    col_prep(breaks = breaks)
  
}