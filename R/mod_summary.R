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
      
      column(3, 
        dateRangeInput(
          inputId = ns("click_date"), 
          label = "Period", 
          start = start_of_this_year(), 
          end = Sys.Date(), 
          weekstart = 1
        )
      ),
      column(3,
        selectizeInput(
          inputId = ns("select_team"), 
          label = "Team", 
          choices = NULL
        )
      )
    ),
    
    fluidRow(
      
      infoBoxOutput(outputId = ns("info_bets"), width = 3),
      infoBoxOutput(outputId = ns("info_stake"), width = 3),
      infoBoxOutput(outputId = ns("info_earnings"), width = 3),
      infoBoxOutput(outputId = ns("info_return"), width = 3)
      
    ),
    
    fluidRow(
      
      tabsetPanel(
        show_earning_panel(
          title = "Bet type", 
          outputId = ns("table_game_type")
        ),
        show_earning_panel(
          title = "Tournament", 
          outputId = ns("table_tournament")
        ),
        show_earning_panel(
          title = "Team", 
          outputId = ns("table_team")
        ),
        show_earning_panel(
          title = "Live betting vs oddset", 
          outputId = ns("table_game")
        ),
        show_earning_panel(
          title = "Odds ranges", 
          outputId = ns("table_odds_group")
        )
      )
    ),
    
    fluidRow(
      shinyjs::useShinyjs(),
      shinyjs::hidden(
        tagList(
          selectInput(
            inputId = ns("select_yaxis"), 
            label = "Y axis", 
            choices = c("Bets", "Stake", "Revenue", "Earnings", "Return"), 
            selected = "Earnings", 
            width = "16.66667%",
          )
        )
      )
    ),
    
    fluidRow(
      
      plotly::plotlyOutput(outputId = ns("plot_earnings"))
      
    )
    
  )
}
    
#' summary Server Functions
#'
#' @noRd 
mod_summary_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data_full <- read_data() %>% 
      dplyr::mutate(HomeTeam = map_game_to_home_team(Kamp), 
                    AwayTeam = map_game_to_away_team(Kamp), 
                    Købsdato = as.Date(Købsdato),
                    Kampdato = as.Date(Kampdato))
    
    observe({
      
      if (exists("data_full")) {
        updateSelectizeInput(
          session = session, 
          inputId = "select_team", 
          choices = c("", get_team_names(data = data_full))
        )
      }
      
    })
    
    data <- reactive({
      data_tmp <- data_full %>% 
        dplyr::filter(Kampdato >= input$click_date[1], Kampdato <= input$click_date[2])
      
      if (input$select_team != "") {
        
        data_tmp <- data_tmp %>% 
          dplyr::filter(HomeTeam == input$select_team | AwayTeam == input$select_team)
  
      }
      
      return(data_tmp)
      
    })
    
    ## Info boxes
    
    df_info <- reactive({
      data() %>% 
        calculate_earnings() %>% 
        dplyr::arrange(dplyr::desc(Bets))
    })
    
    output$info_bets <- renderInfoBox({
      infoBox(
        title = "Bets", 
        value = df_info()$Bets,
        icon = icon("dice")
      )
    })
    
    output$info_stake <- renderInfoBox({
      infoBox(
        title = "Stake", 
        value = df_info()$Stake %>% add_kr(),
        icon = icon("wallet")
      )
    })
    
    output$info_earnings <- renderInfoBox({
      infoBox(
        title = "Earnings", 
        value = df_info()$Earnings %>% add_kr(),
        icon = icon("money-bill-wave")
      )
    })
    
    output$info_return <- renderInfoBox({
      infoBox(
        title = "Return", 
        value = df_info()$Return %>% add_pct(),
        icon = if (df_info()$Return >= 0) icon("arrow-up") else icon("arrow-down")
      )
    })
    
    ## Info boxes end
    
    ## Tables with earnings
    
    df_game_type <- reactive({
      data() %>% 
        dplyr::group_by(Spiltype) %>% 
        calculate_earnings() %>% 
        dplyr::arrange(dplyr::desc(Bets))
    })
    
    output$table_game_type <- DT::renderDataTable({
      DT::datatable(df_game_type(), selection = "single")
    })
    
    df_tournament <- reactive({
      data() %>% 
        dplyr::group_by(Turnering) %>% 
        calculate_earnings() %>% 
        dplyr::arrange(dplyr::desc(Bets))
    })
    
    output$table_tournament <- DT::renderDataTable({
      DT::datatable(df_tournament())
    })
    
    df_team_home <- reactive({
      data() %>% 
        dplyr::filter(!is.na(HomeTeam)) %>% 
        dplyr::group_by(HomeTeam) %>% 
        calculate_earnings() %>% 
        dplyr::arrange(dplyr::desc(Bets)) %>% 
        dplyr::rename(Team = HomeTeam)
    })
    
    df_team_away <- reactive({
      data() %>% 
        dplyr::filter(!is.na(AwayTeam)) %>% 
        dplyr::group_by(AwayTeam) %>% 
        calculate_earnings() %>% 
        dplyr::arrange(dplyr::desc(Bets)) %>% 
        dplyr::rename(Team = AwayTeam)
    })
    
    df_team_combined <- reactive({
      dplyr::bind_rows(
        df_team_home(), 
        df_team_away()
      ) %>% 
        dplyr::group_by(Team) %>% 
        calculate_earnings(var_bets = "Bets", var_stake = "Stake", var_revenue = "Revenue") %>% 
        dplyr::arrange(dplyr::desc(Bets))
    })
    
    output$table_team <- DT::renderDataTable({
      DT::datatable(df_team_combined(), selection = "single")
    })
    
    
    df_game <- reactive({
      data() %>% 
        dplyr::group_by(Spil) %>% 
        calculate_earnings() %>% 
        dplyr::arrange(dplyr::desc(Bets))
    })
    
    output$table_game <- DT::renderDataTable({
      DT::datatable(df_game(), selection = "single")
    })
  
    
    breaks <- c(
      seq(from = 1, to = 2, by = 0.25), 
      seq(from = 2.5, to = 15, by = 2)
    )
    
    df_odds_group <- reactive({
      data() %>% 
        dplyr::mutate(
          Odds = dplyr::if_else(Odds > 5, 5, Odds),
          OddsGroup = cut(Odds, breaks = breaks)
        ) %>% 
        dplyr::group_by(OddsGroup) %>% 
        calculate_earnings() %>% 
        dplyr::arrange(OddsGroup)
    })
    
    output$table_odds_group <- DT::renderDataTable({
      DT::datatable(df_odds_group(), selection = "single")
    })
    
    ## End of tables with earnings
        
    # Plot
    plot_data <- reactive({
      data() %>% 
        dplyr::group_by(Købsdato) %>% 
        calculate_earnings() %>% 
        accumulate_earnings()
    })
    
    output$plot_earnings <- plotly::renderPlotly({
      
      plot_data() %>% 
        plot_earnings(y = input$select_yaxis)
      
    })
    
    observe({
      if (exists("plot_data")) {
        shinyjs::show(id = "select_yaxis")
      }
    })
    
    react_game_type <- reactive({
      
      df_game_type() %>% 
        dplyr::slice(input$table_game_type_rows_selected) %>% 
        dplyr::pull(Spiltype)
      
    })
    
    output$table_game_type_subset <- DT::renderDataTable(
      DT::datatable(get_selected_subset(data = data(), Spiltype == react_game_type()))
    )
    
    observeEvent(input$table_game_type_rows_selected, {
      
      showModal(
        ui = modalDialog(
          DT::dataTableOutput(outputId = ns("table_game_type_subset")), 
          size = "l", 
          easyClose = TRUE
        ), 
        session = session
      )
      
    })
 
  })
}
    
## To be copied in the UI
# mod_summary_ui("summary_ui_1")
    
## To be copied in the server
# mod_summary_server("summary_ui_1")
