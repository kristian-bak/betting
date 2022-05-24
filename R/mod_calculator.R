#' calculator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_calculator_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Smart stake calculator"),
    p("Avoid unnecessary loss of revenue due to rounding"),
    fluidRow(
      column(3, 
        numericInput(
          inputId = ns("num_odds"), 
          label = "Odds", 
          value = 1.37, 
          min = 1, 
          max = Inf, 
          step = 0.05
        )
      ),
      column(3, 
        numericInput(
          inputId = ns("num_stake"), 
          label = "Stake", 
          value = 10, 
          min = 1, 
          max = Inf, 
          step = 0.5
        )
      )
    ),
    fluidRow(
      column(8, 
        DT::dataTableOutput(outputId = ns("table_smart_stake"))
      )
    )
  )
}
    
#' calculator Server Functions
#'
#' @noRd 
mod_calculator_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    df_smart_stake <- reactive({
      calculate_smart_stake(odds = input$num_odds, stake = input$num_stake)
    })
    
    output$table_smart_stake <- DT::renderDataTable({
      DT::datatable(df_smart_stake(), options = list(dom = "t"))
    })
 
  })
}
    
## To be copied in the UI
# mod_calculator_ui("calculator_ui_1")
    
## To be copied in the server
# mod_calculator_server("calculator_ui_1")
