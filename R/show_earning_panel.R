#' Show earning panel
#' @param title character string with title of panel
#' @param outputId outputId of tabel output
#' @param ... additional arguments applied to shiny::tabPanel
show_earning_panel <- function(title, outputId, ...) {
  
  tabPanel(title = title,
           br(),
           DT::dataTableOutput(outputId = outputId), ...)
  
}