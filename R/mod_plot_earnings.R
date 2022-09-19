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
      column(3, 
        shinyjs::hidden(
          selectInput(
            inputId = ns("select_return_x_var"), 
            label = "Select time horizon", 
            choices = c("Year", "Month", "Week"), 
            selected = "Week"
          )
        )
      ),
      column(3, 
        shinyjs::hidden(
          selectInput(
            inputId = ns("select_period_y_var"), 
            label = "Select attribute", 
            choices = c("Bets", "StakeLimit", "MedianOdds", "Stake", "Accuracy", "Revenue", 
                        "Earnings", "Return", "ExpReturn"), 
            selected = "Return"
          )
        )
      )
    ),

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
        dplyr::group_by(dplyr::across(input$select_return_x_var)) %>% 
        calculate_earnings() %>% 
        select_stress(stress = FALSE)
    })
    
    output$plot_return <- plotly::renderPlotly({
      
      plot_data_return() %>% 
        plot_return(
          x = input$select_return_x_var,
          y = input$select_period_y_var
        )
      
    })
    
    output$plot_earnings <- plotly::renderPlotly({
      
      plot_data() %>% 
        plot_earnings(
          y1 = input$select_yaxis1, 
          y2 = input$select_yaxis2
        ) %>% 
        plotly::event_register(event = "plotly_click")
      
    })
    
    observe({
      if (is.data.frame(plot_data())) {
        
        shinyjs::delay(ms = 0.1, expr = {
          shinyjs::show(id = "select_yaxis1")
          shinyjs::show(id = "select_yaxis2")
          shinyjs::show(id = "select_return_x_var")
          shinyjs::show(id = "select_period_y_var")
        })
        
      }
    })
    
    plotly_return_period_clicked <- reactive({
      
      clicks <- plotly::event_data(event = "plotly_click")
      
      if (!is.null(clicks)) {
        clicks %>% 
          dplyr::pull(x)
      }
      
    })
    
    ## Show subset of data for selected period
    observeEvent(plotly_return_period_clicked(), {
      
      showModal(
        ui = modalDialog(
          DT::dataTableOutput(outputId = ns("table_return_subset")), 
          size = "l", 
          easyClose = TRUE
        ), 
        session = session
      )
      
    })
    
    output$table_return_subset <- DT::renderDataTable({
      
      selection_statement <- paste0(input$select_return_x_var, " == ", 
                                    "'", plotly_return_period_clicked(), "'")
      
      DT::datatable(get_selected_subset(data = data(), 
                                        rlang::eval_tidy(rlang::parse_expr(selection_statement))))
      
    })
    
    
  })
}
    
## To be copied in the UI
# mod_plot_earnings_ui("plot_earnings_ui_1")
    
## To be copied in the server
# mod_plot_earnings_server("plot_earnings_ui_1")
