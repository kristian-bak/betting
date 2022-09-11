#' infobox UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_infobox_ui <- function(id){
  ns <- NS(id)
  tagList(
    htmltools::br(),
    fluidRow(
      infoBoxOutput(outputId = ns("info_bets"), width = 3),
      infoBoxOutput(outputId = ns("info_stake"), width = 3),
      infoBoxOutput(outputId = ns("info_earnings"), width = 3),
    ),
    fluidRow(
      infoBoxOutput(outputId = ns("info_return"), width = 3),
      infoBoxOutput(outputId = ns("info_streak"), width = 3),
      infoBoxOutput(outputId = ns("info_accuracy"), width = 3)
    )

  )
}
    
#' infobox Server Functions
#'
#' @noRd 
mod_infobox_server <- function(id, data, stress){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
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
        select_stress(stress()) %>% 
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
 
  })
}
    
## To be copied in the UI
# mod_infobox_ui("infobox_ui_1")
    
## To be copied in the server
# mod_infobox_server("infobox_ui_1")
