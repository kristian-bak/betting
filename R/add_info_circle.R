#' @export
add_info_circle <- function(label, placement = "right", content, return = "HTML") {
  
  out <- htmltools::HTML(
    paste0(label, 
           ' <i class="fa fa-info-circle" data-toggle="tooltip" data-placement=', 
           add_quotes(x = placement), 
           ' title = ', add_quotes(x = content), 
           ' ></i>')
  )
  
  if (return == "character") {
    out <- as.character(out)
  }
  
  return(out)
  
}

add_quotes <- function(x) {
  paste0('"', x, '"')
}