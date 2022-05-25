#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  input_file <- mod_upload_data_server("upload_data_ui_1", data_init = data$data_init)

  data <- mod_filters_server("filters_ui_1", input_file = input_file)
  
  mod_infobox_server("infobox_ui_1", data = data$data)
  
  mod_summary_server(id = "summary_ui_1", data = data$data)
  
  mod_plot_earnings_server("plot_earnings_ui_1", data = data$data)
  
  mod_plot_distribution_server("plot_distribution_ui_1", data = data$data)
  
  mod_calculator_server("calculator_ui_1")
  
}
