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
  tagList(
 
  )
}
    
#' summary Server Functions
#'
#' @noRd 
mod_summary_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data <- read_data()
    
    data %>% 
      calculate_earnings()
    
    data %>% 
      dplyr::group_by(Spiltype) %>% 
      calculate_earnings()
    
    data %>% 
      dplyr::group_by(Turnering) %>% 
      calculate_earnings()
    
    data %>% 
      dplyr::group_by(Spil) %>% 
      calculate_earnings()
    
    breaks <- c(
      seq(from = 1, to = 2, by = 0.25), 
      seq(from = 2.5, to = 15, by = 2)
    )
    
    data %>% 
      dplyr::mutate(
        Odds = dplyr::if_else(Odds > 5, 5, Odds),
        OddsGroup = cut(Odds, breaks = breaks)
      ) %>% 
      dplyr::group_by(OddsGroup) %>% 
      calculate_earnings() %>% 
      dplyr::arrange(OddsGroup)
    
 
  })
}
    
## To be copied in the UI
# mod_summary_ui("summary_ui_1")
    
## To be copied in the server
# mod_summary_server("summary_ui_1")
