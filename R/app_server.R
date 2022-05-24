#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  file_input <- mod_upload_data_server("upload_data_ui_1", data_init = data_init)
  
  data_init  <- mod_summary_server(id = "summary_ui_1", file_input = file_input)
  
  mod_calculator_server("calculator_ui_1")
  
}
