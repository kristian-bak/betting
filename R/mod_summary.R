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
      
      shinyWidgets::materialSwitch(
        inputId = ns("switch_stress"), 
        label = "Stress", 
        value = FALSE,
        status = "primary"
      ),
      
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
          p(glue::glue("Note: odds larger than {bound_odds} are placed in the last interval"))
        ),
        show_earning_panel(
          title = "Bets per match", 
          outputId = ns("table_bets")
        ),
        show_earning_panel(
          title = "Stake", 
          outputId = ns("table_stake"),
          p(glue::glue("Note: stake larger than {bound_stake} are placed in the last interval"))
        ),
        show_earning_panel(
          title = "Country", 
          outputId = ns("table_country")
        ),
        show_earning_panel(
          title = "Bookmaker", 
          outputId = ns("table_bookmaker")
        ),
        show_earning_panel(
          title = "BetDay vs MatchDay", 
          outputId = ns("table_days"),
          p("Clarification: DaysDiff > 0 means the bet was made at least one day before the match day")
        ),
        show_earning_panel(
          title = "Home vs away", 
          outputId = ns("table_home_vs_away")
        )
      )
    )
  )
}
    
#' summary Server Functions
#'
#' @noRd 
mod_summary_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ## Tables with earnings
    
    ## Bet type
    df_game_type <- reactive({
      
      data() %>% 
        calculate_earnings_grouped_by(GameType) %>% 
        select_stress(stress = input$switch_stress)
    })
    
    output$table_game_type <- DT::renderDataTable({
      DT::datatable(df_game_type(), selection = "single") %>% 
        color_by(var = "Return", colors = c("tomato", "white", "lightgreen"))
    })
    
    ## Tournament
    df_tournament <- reactive({
      data() %>% 
        calculate_earnings_grouped_by(Tournament) %>% 
        select_stress(stress = input$switch_stress)
    })
    
    output$table_tournament <- DT::renderDataTable({
      DT::datatable(df_tournament(), selection = "single") %>% 
        color_by(var = "Return", colors = c("tomato", "white", "lightgreen"))
    })
    
    ## Team
    df_team_home <- reactive({
      data() %>% 
        calculate_earnings_grouped_by_team(HomeTeam) %>% 
        select_stress(stress = input$switch_stress)
    })
    
    df_team_away <- reactive({
      data() %>% 
        calculate_earnings_grouped_by_team(AwayTeam) %>% 
        select_stress(stress = input$switch_stress)
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
        calculate_earnings_grouped_by(Game) %>% 
        select_stress(stress = input$switch_stress)
    })
    
    output$table_game <- DT::renderDataTable({
      DT::datatable(df_game(), selection = "single") %>% 
        color_by(var = "Return", colors = c("tomato", "white", "lightgreen"))
    })
    
    ## Odds range
    df_odds_group <- reactive({
      data() %>% 
        calculate_earnings_grouped_by(OddsGroup) %>% 
        arrange_by_odds_group() %>% 
        select_stress(stress = input$switch_stress)
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
        calculate_earnings_grouped_by_stake() %>% 
        select_stress(stress = input$switch_stress)
    })
    
    output$table_stake <- DT::renderDataTable({
      DT::datatable(df_stake(), selection = "single") %>% 
        color_by(var = "Return", colors = c("tomato", "white", "lightgreen"))
    })
    
    ## Country
    df_country <- reactive({
      
      data() %>% 
        calculate_earnings_grouped_by(Country) %>% 
        select_stress(stress = input$switch_stress)
    })
    
    output$table_country <- DT::renderDataTable({
      DT::datatable(df_country(), selection = "single") %>% 
        color_by(var = "Return", colors = c("tomato", "white", "lightgreen"))
    })
    
    ## Bookmaker
    df_bookmaker <- reactive({
      
      data() %>% 
        calculate_earnings_grouped_by(Bookmaker) %>% 
        select_stress(stress = input$switch_stress)
    })
    
    output$table_bookmaker <- DT::renderDataTable({
      DT::datatable(df_bookmaker(), selection = "single") %>% 
        color_by(var = "Return", colors = c("tomato", "white", "lightgreen"))
    })
    
    ## Days between BetDay and MatchDay
    df_days <- reactive({
      
      data() %>% 
        calculate_earnings_grouped_by(DaysDiff) %>% 
        dplyr::filter(Bets > 0) %>% 
        select_stress(stress = input$switch_stress)
      
    })
    
    output$table_days <- DT::renderDataTable({
      DT::datatable(df_days(), selection = "single") %>% 
        color_by(var = "Return", colors = c("tomato", "white", "lightgreen"))
    })
    
    ## Home vs away
    df_home_vs_away <- reactive({
      
      data() %>% 
        calculate_earnings_grouped_by(PredWin) %>% 
        select_stress(stress = input$switch_stress)
      
    })
    
    output$table_home_vs_away <- DT::renderDataTable({
      DT::datatable(df_home_vs_away(), selection = "single") %>% 
        color_by(var = "Return", colors = c("tomato", "white", "lightgreen"))
    })
    
    ## End of tables with earnings
    
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
        slice_and_pull(i = input$table_tournament_rows_selected, j = Tournament)
      
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
        slice_and_pull(i = input$table_team_rows_selected, j = Team)
      
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
        slice_and_pull(i = input$table_game_rows_selected, j = Game)
      
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
        slice_and_pull(i = input$table_odds_group_rows_selected, j = OddsGroup)
      
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
        slice_and_pull(i = input$table_bets_rows_selected, j = BetsInGame)
      
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
        slice_and_pull(i = input$table_stake_rows_selected, j = StakeGroup)
      
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
    
    ## Show subset for country - start
    react_country <- reactive({
      
      df_country() %>% 
        slice_and_pull(i = input$table_country_rows_selected, j = Country)
      
    })
    
    output$table_country_subset <- DT::renderDataTable({
      
      DT::datatable(get_selected_subset(data = data(), Country == react_country()))
      
    })
    
    observeEvent(input$table_country_rows_selected, {
      
      showModal(
        ui = modalDialog(
          DT::dataTableOutput(outputId = ns("table_country_subset")), 
          size = "l", 
          easyClose = TRUE
        ), 
        session = session
      )
      
    })
    
    ## Show subset for country - end
    
    ## Show subset for bookmaker - start
    react_bookmaker <- reactive({
      
      df_bookmaker() %>% 
        slice_and_pull(i = input$table_bookmaker_rows_selected, j = Bookmaker)
      
    })
    
    output$table_bookmaker_subset <- DT::renderDataTable({
      
      DT::datatable(get_selected_subset(data = data(), Bookmaker == react_bookmaker()))
      
    })
    
    observeEvent(input$table_bookmaker_rows_selected, {
      
      showModal(
        ui = modalDialog(
          DT::dataTableOutput(outputId = ns("table_bookmaker_subset")), 
          size = "l", 
          easyClose = TRUE
        ), 
        session = session
      )
      
    })
    
    ## Show subset for bookmaker - end
    
    ## Show subset for Days - start
    react_days <- reactive({
      
      df_days() %>% 
        slice_and_pull(i = input$table_days_rows_selected, j = DaysDiff)
      
    })
    
    output$table_days_subset <- DT::renderDataTable({
      
      DT::datatable(get_selected_subset(data = data(), DaysDiff == react_days()))
      
    })
    
    observeEvent(input$table_days_rows_selected, {
      
      showModal(
        ui = modalDialog(
          DT::dataTableOutput(outputId = ns("table_days_subset")), 
          size = "l", 
          easyClose = TRUE
        ), 
        session = session
      )
      
    })
    
    ## Show subset for Days - end
    
    ## Show subset for Home vs Away - start
    react_home_vs_away <- reactive({
      
      df_home_vs_away() %>% 
        slice_and_pull(i = input$table_home_vs_away_rows_selected, j = PredWin)
      
    })
    
    output$table_home_vs_away_subset <- DT::renderDataTable({

      print(react_home_vs_away())
      
      DT::datatable(get_selected_subset(data = data(), PredWin == react_home_vs_away()))
      
    })
    
    observeEvent(input$table_home_vs_away_rows_selected, {
      
      showModal(
        ui = modalDialog(
          DT::dataTableOutput(outputId = ns("table_home_vs_away_subset")), 
          size = "l", 
          easyClose = TRUE
        ), 
        session = session
      )
      
    })
    
    ## Show subset for Days - end
    
    return(reactive(input$switch_stress))
 
  })
}
    
## To be copied in the UI
# mod_summary_ui("summary_ui_1")
    
## To be copied in the server
# mod_summary_server("summary_ui_1")
