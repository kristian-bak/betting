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
    
    react_var <- reactiveValues()
    
    observe({
      if (is.null(input$browse_file)) {
        react_var$data_loaded   <- data_init
        react_var$data_uploaded <- NULL
      }
    })
    
    observeEvent(input$browse_file, {
      
      react_var$data_uploaded <- readxl::read_excel(path = input$browse_file$datapath)
      
      cols_expected <- c("BetDay", "MatchDay", "Tournament", "Match", "Game",       
                         "Bet", "Odds", "Correct", "Stake", "Revenue", "GameType")
      
      cols_present <- cols_expected %in% names(react_var$data_uploaded)
      
      typeof_vec <- sapply(react_var$data_uploaded, typeof)
      
      typeof_expected <- c("double", "double", "character", "character", "character", "character", 
                           "double", "double", "double", "double", "character")
      
      typeof_matching <- typeof_vec == typeof_expected
      
      if (!all(typeof_matching)) {
        
        typeof_miss_match <- cols_expected[which(!typeof_matching)[1]]
        
        shinyWidgets::show_alert(
          title = "Error", 
          text = paste0("Wrong class for column ", typeof_miss_match),
          type = "error"
        )
        
        react_var$data_loaded   <- data_init
        react_var$data_uploaded <- NULL
        
        return()
        
      }
      
      if (!all(cols_present)) {
        
        col_miss <- cols_expected[which(!cols_present)[1]]
        
        shinyWidgets::show_alert(
          title = "Error", 
          text = paste0("Input data must include column ", col_miss),
          type = "error"
        )
        
        react_var$data_loaded   <- data_init
        react_var$data_uploaded <- NULL
        
        return()
        
      }
      
      react_var$data_loaded <- react_var$data_uploaded
      
    })
    
    output$table_input <- DT::renderDataTable({{
      DT::datatable(react_var$data_loaded)
    }})
    
    #out <- reactive(input$browse_file)
    
    #return(out)
    return(reactive(react_var$data_uploaded))
 
  })
}
    
## To be copied in the UI
# mod_upload_data_ui("upload_data_ui_1")
    
## To be copied in the server
# mod_upload_data_server("upload_data_ui_1")
