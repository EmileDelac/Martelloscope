#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  r <- reactiveValues()
  mod_production_server("production_ui_1", r)
  mod_result_server("result_ui_1", r)
  mod_qfield_server("qfield_ui_1", r)
  mod_accueil_server("accueil_ui_1", r)
  mod_ressources_server("ressources_ui_1", r)
}
