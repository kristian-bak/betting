#' plot_distribution UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_distribution_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(3, 
        selectInput(
          inputId = ns("select_covariate"), 
          label = "Covariate", 
          choices = c("Game", "GameType", "Odds", "Revenue", "Stake", "Tournament")
        )
      )
    ),
    
    fluidRow(
      
      verbatimTextOutput(outputId = ns("show_summary"))
      
    ),
    
    fluidRow(
      
      plotly::plotlyOutput(outputId = ns("plot_distribution"))
      
    )
  )
}
    
#' plot_distribution Server Functions
#'
#' @noRd 
mod_plot_distribution_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$plot_distribution <- plotly::renderPlotly({
      
      data() %>% 
        plot_distribution(var = input$select_covariate)
      
    })
    
    output$show_summary <- renderPrint({
      
      data() %>% 
        dplyr::pull(input$select_covariate) %>% 
        summarise_distribution()
      
    })
    
    observe({
      
      hide_distribution <- data() %>% 
        dplyr::pull(input$select_covariate) %>% 
        is.character()
      
      if (hide_distribution) {
        shinyjs::hide(id = "show_summary")
      } else {
        shinyjs::show(id = "show_summary")
      }
      
    })
 
  })
}
    
## To be copied in the UI
# mod_plot_distribution_ui("plot_distribution_ui_1")
    
## To be copied in the server
# mod_plot_distribution_server("plot_distribution_ui_1")
