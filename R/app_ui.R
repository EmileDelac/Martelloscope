#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your application UI logic 
    fluidPage(
      div(class = "navbar1", navbarPage("Marteloscope",
                                        tabPanel("Accueil", icon = icon("home"),
                                                 mod_accueil_ui("accueil_ui_1")),
                                        
                                        tabPanel("Caract\u00e9risation", icon = icon("tree"),
                                                 mod_production_ui("production_ui_1")),
                                        
                                        tabPanel("Qfield", icon = icon("mountain"),
                                                 mod_qfield_ui("qfield_ui_1")),
                                        
                                        tabPanel("R\u00e9sultats", icon = icon("bar-chart-o",verify_fa = FALSE),
                                                 mod_result_ui("result_ui_1")),
                                        
                                        tabPanel("Ressources", icon = icon("book",verify_fa = FALSE),
                                                 mod_ressources_ui("ressources_ui_1")),
                                        
                                
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
      app_title = 'marteloscope5'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

