#' upload_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_upload_data_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(6, 
             fileInput(inputId = ns("browse_file"), 
                       label = "Select excel file", 
                       accept = "xlsx")
      )
    ), 
    h3("Loaded data"),
    fluidRow(
      DT::dataTableOutput(outputId = ns("table_input"))
    )
  )

}
    
#' upload_data Server Functions
#'
#' @noRd 
mod_upload_data_server <- function(id, data_init){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    react_var <- reactiveValues(data = NULL)
    
    observe({
      
      if (!is.null(input$browse_file)) {
        react_var$data     <- readxl::read_excel(path = input$browse_file$datapath)
        react_var$data_out <- react_var$data
      } else {
        react_var$data     <- data_init
        react_var$data_out <- NULL
      }
      
    })
    
    output$table_input <- DT::renderDataTable({{
      DT::datatable(react_var$data)
    }})
    
    output$table_init_data <- DT::renderDataTable({{
      DT::datatable(data_init)
    }})
    
    out <- reactive(input$browse_file)
    
    return(out)
 
  })
}
    
## To be copied in the UI
# mod_upload_data_ui("upload_data_ui_1")
    
## To be copied in the server
# mod_upload_data_server("upload_data_ui_1")
