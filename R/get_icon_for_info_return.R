#' Get icon for info return
#' @param x numeric scalar
#' 
get_icon_for_info_return <- function(x) {
  
  if (x == 0) {
    
    out <- icon("question")
    
  } else if (x >= 0) {
   
    out <- icon("arrow-up")
    
  } else {
    
    out <- icon("arrow-down")

  }
  
  return(out)
  
}