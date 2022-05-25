#' Show dropdown box
#' 
show_dropdown_box <- function(ns) {
  
  shinyWidgets::dropdownButton(
    inputId = ns("go_drop_down"),
    circle = TRUE, 
    status = "primary", 
    icon = icon("cog"),
    tagList(
      selectizeInput(
        inputId = ns("select_tournament"), 
        label = "Tournament", 
        choices = "", 
        multiple = TRUE,
        options = list(
          'plugins' = list('remove_button')
        )
      ),
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
      ),
      selectizeInput(
        inputId = ns("select_game_type"), 
        label = "Bet type", 
        choices = NULL,
        multiple = TRUE,
        options = list(
          'plugins' = list('remove_button')
        )
      ),
      selectizeInput(
        inputId = ns("select_game"), 
        label = "Live betting vs oddset", 
        choices = c("", "Live Betting", "Oddset")
      ),
      shinyWidgets::materialSwitch(
        inputId = ns("switch_lings_together"), 
        label = "Group lings together", 
        value = FALSE, 
        status = "primary"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns("switch_dbl_tp_as_lings"), 
        label = "Group double and triple as lings", 
        value = FALSE, 
        status = "primary"
      )
    )
  )
  
}