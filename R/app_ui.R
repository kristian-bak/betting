library(shinydashboard)
library(shinyBS)
#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
  # Leave this function for adding external resources
  golem_add_external_resources()
  
  shinydashboard::dashboardPage(
    dashboardHeader(title = "Betting"),
    dashboardSidebar(
      
      show_version_number(),
      
      sidebarMenu(
        menuItem("Overview", tabName = "dashboard", icon = icon("chart-line")),
        menuItem("Upload data", tabName = "upload", icon = icon("upload"))
      )
      
    ),
    
    dashboardBody(
      
      tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                
                mod_summary_ui("summary_ui_1")
                
        ),
        
        # Second tab content
        tabItem(tabName = "upload",
                
                mod_upload_data_ui("upload_data_ui_1")
                
        )
      )
    )
    
  )
  
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Betting'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

