#' plot_earnings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_earnings_ui <- function(id){
  ns <- NS(id)
  
  tagList(

    fluidRow(
      
      plotly::plotlyOutput(outputId = ns("plot_return"))
      
    ),
    
    htmltools::br(),
    
    fluidRow(
      column(3, 
             shinyjs::hidden(
               selectInput(
                 inputId = ns("select_yaxis1"), 
                 label = "Y axis 1", 
                 choices = c("Bets", "Stake", "Revenue", "Earnings", "Return"), 
                 selected = "Earnings"
               )
             )
      ),
      column(3, 
             shinyjs::hidden(
               selectizeInput(
                 inputId = ns("select_yaxis2"), 
                 label = "Y axis 2", 
                 choices = c("", "Bets", "Stake", "Revenue", "Earnings", "Return")
               )
             )
      )
    ),
    
    fluidRow(
      
      plotly::plotlyOutput(outputId = ns("plot_earnings"))
      
    )
  )
  
}
    
#' plot_earnings Server Functions
#'
#' @noRd 
mod_plot_earnings_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # Plot
    plot_data <- reactive({
      data() %>% 
        dplyr::group_by(BetDay) %>% 
        calculate_earnings() %>% 
        accumulate_earnings()
    })
    
    plot_data_return <- reactive({
      data() %>% 
        dplyr::mutate(Year = substring(BetDay, 1, 4),
                      Month = substring(BetDay, 1, 7), 
                      Week = data.table::week(BetDay)) %>% 
        dplyr::group_by(Month) %>% 
        calculate_earnings() %>% 
        select_stress(stress = FALSE)
    })
    
    output$plot_return <- plotly::renderPlotly({
      
      plot_data_return() %>% 
        plot_return(
          x = "Month"
        )
      
    })
    
    output$plot_earnings <- plotly::renderPlotly({
      
      plot_data() %>% 
        plot_earnings(
          y1 = input$select_yaxis1, 
          y2 = input$select_yaxis2
        )
      
    })
    
    observe({
      if (is.data.frame(plot_data())) {
        
        shinyjs::delay(ms = 0.1, expr = {
          shinyjs::show(id = "select_yaxis1")
          shinyjs::show(id = "select_yaxis2")
        })
        
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_plot_earnings_ui("plot_earnings_ui_1")
    
## To be copied in the server
# mod_plot_earnings_server("plot_earnings_ui_1")
