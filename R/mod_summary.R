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
      ), 
      column(2,
        selectizeInput(
          inputId = ns("select_tournament"), 
          label = "Tournament", 
          choices = NULL
        )
      )
    ),
    
    fluidRow(
      actionButton(
        inputId = ns("go_last_month"), 
        label = "Last month", 
        style = 'padding:4px; font-size:89%; margin-left: 12px'
      ),
      actionButton(
        inputId = ns("go_last_3_months"), 
        label = "Last 3 months", 
        style = 'padding:4px; font-size:89%'
      ),
      actionButton(
        inputId = ns("go_this_year"), 
        label = get_this_year(), 
        style = 'padding:4px; font-size:89%'
      ),
      actionButton(
        inputId = ns("go_all_time"), 
        label = "All time", 
        style = 'padding:4px; font-size:89%'
      )
    ),
    
    fluidRow(
      column(3, 
        sliderInput(
          inputId = ns("slide_stake"), 
          label = "Stake", 
          min = 0, 
          max = 500, 
          value = c(0, 500), 
          step = 5, 
          dragRange = TRUE
        )
      ),
      column(3, 
        selectizeInput(
          inputId = ns("select_game_type"), 
          label = "Bet type", 
          choices = NULL
        )
      ),
      column(2, 
        selectizeInput(
          inputId = ns("select_game"), 
          label = "Live betting vs oddset", 
          choices = c("", "Live Betting", "Oddset")
        )
      ),
      column(1, 
        actionButton(
          inputId = ns("go_clear_filters"), 
          label = NULL, 
          icon = icon("times-circle"), 
          style = "margin-top: 24px"
        ),
        shinyBS::bsTooltip(
          id = ns("go_clear_filters"), 
          title = "Reset all filters"
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
    
    breaks <- c(
      seq(from = 1, to = 2, by = 0.25), 
      seq(from = 2.5, to = 15, by = 2)
    )
    
    data_full <- read_data() %>% 
      dplyr::mutate(HomeTeam  = map_game_to_home_team(Kamp), 
                    AwayTeam  = map_game_to_away_team(Kamp), 
                    OddsMod   = dplyr::if_else(Odds > 5, 5, Odds),
                    OddsGroup = cut(OddsMod, breaks = breaks) %>% as.character(),
                    Købsdato  = as.Date(Købsdato),
                    Kampdato  = as.Date(Kampdato))
    
    if (max(data_full$Indsats, na.rm = TRUE) > 500) {
      warning("sliderinput named slide_stake has max hardcoded as 500. Change this to a update statement")
    }
    
    observeEvent(input$go_this_year, {
      
      updateDateRangeInput(
        session = session, 
        inputId = "click_date", 
        start = start_of_this_year(), 
        end = Sys.Date()
      )
      
    })
    
    observeEvent(input$go_all_time, {
      
      updateDateRangeInput(
        session = session, 
        inputId = "click_date", 
        start = min(data_full$Købsdato), 
        end = Sys.Date()
      )
      
    })
    
    observeEvent(input$go_last_month, {
      
      updateDateRangeInput(
        session = session, 
        inputId = "click_date", 
        start = one_month_ago(), 
        end = Sys.Date()
      )
      
    })
    
    observeEvent(input$go_last_3_months, {
      
      updateDateRangeInput(
        session = session, 
        inputId = "click_date", 
        start = three_months_ago(), 
        end = Sys.Date()
      )
      
    })
    
    observeEvent(input$go_clear_filters, {
      
      updateDateRangeInput(
        session = session, 
        inputId = "click_date", 
        start = start_of_this_year(), 
        end = Sys.Date()
      )
      
      updateSelectizeInput(
        session = session, 
        inputId = "select_team", 
        selected = ""
      )
      
      updateSelectizeInput(
        session = session, 
        inputId = "select_tournament", 
        selected = ""
      )
      
      updateSelectizeInput(
        session = session, 
        inputId = "select_game_type", 
        selected = ""
      )
      
      updateSelectizeInput(
        session = session, 
        inputId = "select_game", 
        selected = ""
      )
      
      updateSliderInput(
        session = session, 
        inputId = "slide_stake", 
        value = c(0, 500)
      )
      
    })
    
    data <- reactive({
      data_tmp <- data_full %>% 
        dplyr::filter(Kampdato >= input$click_date[1], Kampdato <= input$click_date[2]) %>% 
        dplyr::filter(Indsats >= input$slide_stake[1], Indsats <= input$slide_stake[2])
      
      if (input$select_team != "") {
        
        data_tmp <- data_tmp %>% 
          dplyr::filter(HomeTeam == input$select_team | AwayTeam == input$select_team)
  
      }
      
      if (input$select_game != "") {
        
        data_tmp <- data_tmp %>% 
          dplyr::filter(Spil == input$select_game)
        
      }
      
      if (input$select_tournament != "") {
        
        data_tmp <- data_tmp %>% 
          dplyr::filter(Turnering == input$select_tournament)
        
      }
      
      if (input$select_game_type != "") {
        
        data_tmp <- data_tmp %>% 
          dplyr::filter(Spiltype == input$select_game_type)
        
      }
      
      return(data_tmp)
      
    })
    
    observe({
      
      if (input$select_team != "") {
        return()
      }
      
      updateSelectizeInput(
        session = session, 
        inputId = "select_team", 
        choices = c("", get_team_names(data = data()))
      )
      
    })
    
    observe({
      
      if (input$select_game_type != "") {
        return()
      }
      
      updateSelectizeInput(
        session = session, 
        inputId = "select_game_type", 
        choices = c("", get_game_types(data = data()))
      )
      
    })
    
    observe({
      
      if (input$select_tournament != "") {
        return()
      }
      
      updateSelectizeInput(
        session = session, 
        inputId = "select_tournament", 
        choices = c("", get_tournament_names(data = data()))
      )
      
    })
    
    observe({
      
      if (input$select_game != "") {
        return()
      }
      
      updateSelectizeInput(
        session = session, 
        inputId = "select_game", 
        choices = c("", get_games(data = data()))
      )
      
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
        icon = get_icon_for_info_return(x = df_info()$Return)
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
    
    df_odds_group <- reactive({
      data() %>% 
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
    
    ## Show subset for game type - start
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
    
    ## Show subset for game type - end
    
    ## Show subset for tournament - start
    react_tournament <- reactive({
      
      df_tournament() %>% 
        dplyr::slice(input$table_tournament_rows_selected) %>% 
        dplyr::pull(Turnering)
      
    })
    
    output$table_tournament_subset <- DT::renderDataTable(
      DT::datatable(get_selected_subset(data = data(), Turnering == react_tournament()))
    )
    
    observeEvent(input$table_tournament_rows_selected, {
      
      showModal(
        ui = modalDialog(
          DT::dataTableOutput(outputId = ns("table_tournament_subset")), 
          size = "l", 
          easyClose = TRUE
        ), 
        session = session
      )
      
    })
    
    ## Show subset for tournament - end
    
    ## Show subset for team - start
    react_team <- reactive({
      
      df_team_combined() %>% 
        dplyr::slice(input$table_team_rows_selected) %>% 
        dplyr::pull(Team)
      
    })
    
    output$table_team_subset <- DT::renderDataTable(
      DT::datatable(
        get_selected_subset(
          data = data(), 
          HomeTeam == react_team() | AwayTeam == react_team()
        )
      )
    )
    
    observeEvent(input$table_team_rows_selected, {
      
      showModal(
        ui = modalDialog(
          DT::dataTableOutput(outputId = ns("table_team_subset")), 
          size = "l", 
          easyClose = TRUE
        ), 
        session = session
      )
      
    })
    
    ## Show subset for team - end
    
    ## Show subset for live betting vs oddset - start
    react_game <- reactive({
      
      df_game() %>% 
        dplyr::slice(input$table_game_rows_selected) %>% 
        dplyr::pull(Spil)
      
    })
    
    output$table_game_subset <- DT::renderDataTable(
      DT::datatable(get_selected_subset(data = data(), Spil == react_game()))
    )
    
    observeEvent(input$table_game_rows_selected, {
      
      showModal(
        ui = modalDialog(
          DT::dataTableOutput(outputId = ns("table_game_subset")), 
          size = "l", 
          easyClose = TRUE
        ), 
        session = session
      )
      
    })
    
    ## Show subset for live betting vs oddset - end
    
    ## Show subset for odds range - start
    react_odds_group <- reactive({
      
      df_odds_group() %>% 
        dplyr::slice(input$table_odds_group_rows_selected) %>% 
        dplyr::pull(OddsGroup)
      
    })
    
    output$table_odds_group_subset <- DT::renderDataTable({
      
      DT::datatable(get_selected_subset(data = data(), OddsGroup == react_odds_group()))
      
    })
    
    observeEvent(input$table_odds_group_rows_selected, {
      
      showModal(
        ui = modalDialog(
          DT::dataTableOutput(outputId = ns("table_odds_group_subset")), 
          size = "l", 
          easyClose = TRUE
        ), 
        session = session
      )
      
    })
    
    ## Show subset for odds range - end
 
  })
}
    
## To be copied in the UI
# mod_summary_ui("summary_ui_1")
    
## To be copied in the server
# mod_summary_server("summary_ui_1")
