#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summary_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    
    fluidRow(
      
      infoBoxOutput(outputId = ns("info_bets"), width = 3),
      infoBoxOutput(outputId = ns("info_stake"), width = 3),
      infoBoxOutput(outputId = ns("info_earnings"), width = 3),
      infoBoxOutput(outputId = ns("info_return"), width = 3)
      
    ),
    
    fluidRow(
      
      tabsetPanel(
        show_earning_panel(
          title = "Game type", 
          outputId = ns("table_game_type")
        ),
        show_earning_panel(
          title = "Tournament", 
          outputId = ns("table_tournament")
        ),
        show_earning_panel(
          title = "Game", 
          outputId = ns("table_game")
        ),
        show_earning_panel(
          title = "Odds", 
          outputId = ns("table_odds_group")
        )
      )
    )
  )
}
    
#' summary Server Functions
#'
#' @noRd 
mod_summary_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data <- read_data()
    
    ## Info boxes
    
    df_info <- data %>% 
      calculate_earnings(order_by = Bets)
    
    output$info_bets <- renderInfoBox({
      infoBox(
        title = "Bets", 
        value = df_info$Bets,
        icon = icon("dice")
      )
    })
    
    output$info_stake <- renderInfoBox({
      infoBox(
        title = "Stake", 
        value = df_info$Stake %>% add_kr(),
        icon = icon("wallet")
      )
    })
    
    output$info_earnings <- renderInfoBox({
      infoBox(
        title = "Earnings", 
        value = df_info$Earnings %>% add_kr(),
        icon = icon("money-bill-wave")
      )
    })
    
    output$info_return <- renderInfoBox({
      infoBox(
        title = "Return", 
        value = df_info$Return %>% add_pct(),
        icon = if (df_info$Return >= 0) icon("chart-line") else icon("chart-line-down")
      )
    })
    
    ## Info boxes end
    
    ## Tables with earnings
    
    df_game_type <- data %>% 
      dplyr::group_by(Spiltype) %>% 
      calculate_earnings(order_by = Bets)
    
    output$table_game_type <- DT::renderDataTable({
      DT::datatable(df_game_type)
    })
    
    df_tournament <- data %>% 
      dplyr::group_by(Turnering) %>% 
      calculate_earnings(order_by = Bets)
    
    output$table_tournament <- DT::renderDataTable({
      DT::datatable(df_tournament)
    })
    
    df_game <- data %>% 
      dplyr::group_by(Spil) %>% 
      calculate_earnings(order_by = Bets)
    
    output$table_game <- DT::renderDataTable({
      DT::datatable(df_game)
    })
  
    
    breaks <- c(
      seq(from = 1, to = 2, by = 0.25), 
      seq(from = 2.5, to = 15, by = 2)
    )
    
    df_odds_group <- data %>% 
      dplyr::mutate(
        Odds = dplyr::if_else(Odds > 5, 5, Odds),
        OddsGroup = cut(Odds, breaks = breaks)
      ) %>% 
      dplyr::group_by(OddsGroup) %>% 
      calculate_earnings(order_by = OddsGroup) %>% 
      dplyr::arrange(OddsGroup)
    
    output$table_odds_group <- DT::renderDataTable({
      DT::datatable(df_odds_group)
    })
    
    ## End of tables with earnings
        
    # Plot
    
    data %>% 
      dplyr::group_by(Købsdato) %>% 
      calculate_earnings(order_by = Bets) %>% 
      accumulate_earnings() %>% 
      plot_earnings(y = "Earnings")
    
 
  })
}
    
## To be copied in the UI
# mod_summary_ui("summary_ui_1")
    
## To be copied in the server
# mod_summary_server("summary_ui_1")
