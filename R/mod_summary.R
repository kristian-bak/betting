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
    
    tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
    
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
          choices = NULL,
          multiple = TRUE,
          options = list(
            'plugins' = list('remove_button')
          )
        )
      ), 
      column(3,
        selectizeInput(
          inputId = ns("select_tournament"), 
          label = "Tournament", 
          choices = "", 
          multiple = TRUE,
          options = list(
            'plugins' = list('remove_button')
          )
        )
      ),
      infoBoxOutput(outputId = ns("info_streak"), width = 3)
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
          label = add_info_circle(
            label = "Stake", 
            placement = "right", 
            content = "Money in DKK placed on a bet"
          ), 
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
          choices = NULL,
          multiple = TRUE,
          options = list(
            'plugins' = list('remove_button')
          )
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
      ),
      infoBoxOutput(outputId = ns("info_accuracy"), width = 3)
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
          outputId = ns("table_odds_group"),
          p("Note: odds larger than 5 are placed in the interval (4, 5]")
        ),
        show_earning_panel(
          title = "Bets per match", 
          outputId = ns("table_bets")
        ),
        show_earning_panel(
          title = "Stake", 
          outputId = ns("table_stake"),
          p("Note: stake larger than 200 are placed in the interval (100, 200]")
        )
      )
    ),
    
    fluidRow(
      shinyjs::useShinyjs(),
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
      
    ),
    
    fluidRow(
      shinyjs::hidden(
        tagList(
          selectInput(
            inputId = ns("select_covariate"), 
            label = "Covariate", 
            choices = c("Game", "GameType", "Odds", "Revenue", "Stake", "Tournament"),
            width = "16.66667%"
          )
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
    
#' summary Server Functions
#'
#' @noRd 
mod_summary_server <- function(id, file_input){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data_init_load <- read_and_prep_data()
    
    if (max(data_init_load$Stake, na.rm = TRUE) > 500) {
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
        start = min(data_init_load$BetDay), 
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

      if (!is.null(file_input())) {

        data_full <- file_input() %>% 
          col_prep()
          
        #data_full <- readxl::read_excel(path = file_input()$datapath) %>% 
        #  col_prep()
          
      } else {
        
        data_full <- data_init_load
        
      }
      
      data_tmp <- data_full %>% 
        dplyr::filter(MatchDay >= input$click_date[1], MatchDay <= input$click_date[2]) %>% 
        dplyr::filter((Stake >= input$slide_stake[1] & Stake <= input$slide_stake[2]) | is.na(Stake))
      
      if (!is.null(input$select_team)) {
        
        data_tmp <- data_tmp %>% 
          dplyr::filter(HomeTeam %in% input$select_team | AwayTeam %in% input$select_team)
  
      }
      
      if (input$select_game != "") {
        
        data_tmp <- data_tmp %>% 
          dplyr::filter(Game == input$select_game)
        
      }
      
      if (!is.null(input$select_tournament)) {
        
        data_tmp <- data_tmp %>% 
          dplyr::filter(Tournament %in% input$select_tournament)
        
      }
      
      if (!is.null(input$select_game_type)) {
        
        data_tmp <- data_tmp %>% 
          dplyr::filter(GameType %in% input$select_game_type)
        
      }
      
      return(data_tmp)
      
    })
    
    observe({
      
      if (is.null(input$select_team)) {
        updateSelectizeInput(
          session = session, 
          inputId = "select_team", 
          choices = c("", get_team_names(data = data()))
        )
      }
      
    })
    
    observe({
      
      if (is.null(input$select_game_type)) {
        updateSelectizeInput(
          session = session, 
          inputId = "select_game_type", 
          choices = c("", get_game_types(data = data()))
        )
      }
      
    })
    
    observe({
            
      if (is.null(input$select_tournament)) {
        updateSelectizeInput(
          session = session, 
          inputId = "select_tournament", 
          choices = c("", get_tournament_names(data = data()))
        )
      }
      
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
    
    winning_streak <- reactive({
      
      winning_streak <- data() %>% 
        dplyr::arrange(ID) %>% 
        dplyr::mutate(WinningStreak = cumsum_with_reset(x = Correct)) %>% 
        dplyr::pull(WinningStreak)
    
    longest_winning_streak <- max(winning_streak)
    current_winning_streak <- winning_streak %>% purrr::pluck(length(.))
    
    out <- list("longest_winning_streak" = longest_winning_streak, 
                "current_winning_streak" = current_winning_streak)
    
    return(out)
    
    })
    
    output$info_streak <- renderInfoBox({
      infoBox(
        title = "Winning streak",
        value = paste0(winning_streak()$current_winning_streak, " / ",
                       winning_streak()$longest_winning_streak),
        subtitle = "Current / longest",
        icon = icon("bolt"), 
      )
    })
    
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
    
    output$info_accuracy <- renderInfoBox({
      infoBox(
        title = "Accuracy", 
        value = df_info()$Accuracy %>% add_pct(),
        icon = icon("check-circle")
      )
    })
    
    ## Info boxes end
    
    ## Tables with earnings
    
    ## Bet type
    df_game_type <- reactive({
      data() %>% 
        dplyr::group_by(GameType) %>% 
        calculate_earnings() %>% 
        dplyr::arrange(dplyr::desc(Bets))
    })
    
    output$table_game_type <- DT::renderDataTable({
      DT::datatable(df_game_type(), selection = "single") %>% 
        color_by(var = "Return", colors = c("tomato", "white", "lightgreen"))
    })
    
    ## Tournament
    df_tournament <- reactive({
      data() %>% 
        dplyr::group_by(Tournament) %>% 
        calculate_earnings() %>% 
        dplyr::arrange(dplyr::desc(Bets))
    })
    
    output$table_tournament <- DT::renderDataTable({
      DT::datatable(df_tournament(), selection = "single") %>% 
        color_by(var = "Return", colors = c("tomato", "white", "lightgreen"))
    })
    
    ## Team
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
        calculate_earnings_for_teams()
    })
    
    output$table_team <- DT::renderDataTable({
      DT::datatable(df_team_combined(), selection = "single") %>% 
        color_by(var = "Return", colors = c("tomato", "white", "lightgreen"))
    })
    
    ## Oddset vs live betting
    df_game <- reactive({
      data() %>% 
        dplyr::group_by(Game) %>% 
        calculate_earnings() %>% 
        dplyr::arrange(dplyr::desc(Bets))
    })
    
    output$table_game <- DT::renderDataTable({
      DT::datatable(df_game(), selection = "single") %>% 
        color_by(var = "Return", colors = c("tomato", "white", "lightgreen"))
    })
    
    ## Odds range
    df_odds_group <- reactive({
      data() %>% 
        dplyr::group_by(OddsGroup) %>% 
        calculate_earnings() %>%
        arrange_by_odds_group()
    })
    
    output$table_odds_group <- DT::renderDataTable({
      DT::datatable(df_odds_group(), selection = "single") %>% 
        color_by(var = "Return", colors = c("tomato", "white", "lightgreen"))
    })
    
    ## Bets per match
    df_bets <- reactive({
      data() %>% 
        calculate_earnings_for_bets_in_game()
    })
    
    output$table_bets <- DT::renderDataTable({
      DT::datatable(df_bets(), selection = "single") %>% 
        color_by(var = "Return", colors = c("tomato", "white", "lightgreen"))
    })
    
    ## Stake
    df_stake <- reactive({
      
      data() %>% 
        dplyr::filter(Stake > 0) %>% 
        dplyr::mutate(
          StakeMidpoint = kb.utils::cut_var(
            x = StakeMod, 
            breaks = c(0, 25, 50, 75, 100, 200)
          )
        ) %>% 
        dplyr::group_by(StakeGroup, StakeMidpoint) %>% 
        calculate_earnings() %>% 
        dplyr::arrange(StakeMidpoint) %>% 
        dplyr::select(-StakeMidpoint)
    })
    
    output$table_stake <- DT::renderDataTable({
      DT::datatable(df_stake(), selection = "single") %>% 
        color_by(var = "Return", colors = c("tomato", "white", "lightgreen"))
    })
    
    
    ## End of tables with earnings
        
    # Plot
    plot_data <- reactive({
      data() %>% 
        dplyr::group_by(BetDay) %>% 
        calculate_earnings() %>% 
        accumulate_earnings()
    })
    
    output$plot_earnings <- plotly::renderPlotly({
      
      plot_data() %>% 
        plot_earnings(
          y1 = input$select_yaxis1, 
          y2 = input$select_yaxis2
        )
      
    })
    
    observe({
      if (is.data.frame(df_game_type())) {
        
        shinyjs::delay(ms = 0.1, expr = {
          shinyjs::show(id = "select_yaxis1")
          shinyjs::show(id = "select_yaxis2")
          shinyjs::show(id = "select_covariate")
        })
        
      }
    })
    
    ## Show subset for game type - start
    react_game_type <- reactive({
      
      df_game_type() %>% 
        dplyr::slice(input$table_game_type_rows_selected) %>% 
        dplyr::pull(GameType)
      
    })
    
    output$table_game_type_subset <- DT::renderDataTable(
      DT::datatable(
        get_selected_subset(
          data = data(), 
          GameType == react_game_type(), 
          var = "GameType",
          value = react_game_type()
        )
      )
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
        dplyr::pull(Tournament)
      
    })
    
    output$table_tournament_subset <- DT::renderDataTable(
      DT::datatable(get_selected_subset(data = data(), Tournament == react_tournament()))
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
        dplyr::pull(Game)
      
    })
    
    output$table_game_subset <- DT::renderDataTable(
      DT::datatable(get_selected_subset(data = data(), Game == react_game()))
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
    
    ## Show subset for bets per match - start
    react_bets <- reactive({
      
      df_bets() %>% 
        dplyr::slice(input$table_bets_rows_selected) %>% 
        dplyr::pull(BetsInGame)
      
    })
    
    output$table_bets_subset <- DT::renderDataTable({
      
      DT::datatable(get_selected_subset(data = data(), Bets == react_bets()))
      
    })
    
    observeEvent(input$table_bets_rows_selected, {
      
      showModal(
        ui = modalDialog(
          DT::dataTableOutput(outputId = ns("table_bets_subset")), 
          size = "l", 
          easyClose = TRUE
        ), 
        session = session
      )
      
    })
    
    ## Show subset for bets per match - end
    
    ## Show subset for stake - start
    react_stake <- reactive({
      
      df_stake() %>% 
        dplyr::slice(input$table_stake_rows_selected) %>% 
        dplyr::pull(StakeGroup)
      
    })
    
    output$table_stake_subset <- DT::renderDataTable({
      
      DT::datatable(get_selected_subset(data = data(), StakeGroup == react_stake()))
      
    })
    
    observeEvent(input$table_stake_rows_selected, {
      
      showModal(
        ui = modalDialog(
          DT::dataTableOutput(outputId = ns("table_stake_subset")), 
          size = "l", 
          easyClose = TRUE
        ), 
        session = session
      )
      
    })
    
    ## Show subset for stake - end
    
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
    
    return(data_init_load)
 
  })
}
    
## To be copied in the UI
# mod_summary_ui("summary_ui_1")
    
## To be copied in the server
# mod_summary_server("summary_ui_1")
