#' filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_filters_ui <- function(id){
  ns <- NS(id)
  tagList(
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
          options = list('plugins' = list('remove_button'))
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
      column(1, 
             show_dropdown_box(ns = ns),
             shinyBS::bsTooltip(
               id = ns("go_drop_down"), 
               title = "Additional filters", 
               placement = "right"
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
    )
  )
}
    
#' filters Server Functions
#'
#' @noRd 
mod_filters_server <- function(id, input_file){
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
      
      if (!is.null(input_file())) {
        
        data_full <- input_file() %>% 
          col_prep()
        
      } else {
        
        data_full <- data_init_load
        
      }
      
      data_tmp <- data_full %>% 
        dplyr::filter(MatchDay >= input$click_date[1], MatchDay <= input$click_date[2]) %>% 
        dplyr::filter((Stake >= input$slide_stake[1] & Stake <= input$slide_stake[2]) | is.na(Stake)) %>% 
        dplyr::mutate(
          GameType = group_lings_together(x = GameType, group = input$switch_lings_together),
          GameType = group_double_and_triple_as_lings(
            x = GameType, 
            group = input$switch_lings_together * input$switch_dbl_tp_as_lings
          )
        )
      
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
    
    out <- list(
      "data_init_load" = data_init_load, 
      "data"           = data
    )
    
    return(out)
 
  })
}
    
## To be copied in the UI
# mod_filters_ui("filters_ui_1")
    
## To be copied in the server
# mod_filters_server("filters_ui_1")
