#' ressources UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ressources_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Notice d'utilisation de l'application"),
    downloadButton(outputId = ns("downloadNotice"), label = "T\u00e9l\u00e9charger Notice"),
    hr(),
    h4("Quelques d\u00e9finitions pour les participants"),
    h6("Accroissement, Am\u00e9lioration, Arbre objectif,..."),
    downloadButton(outputId = ns("downloaddef"), label = "T\u00e9l\u00e9charger D\u00e9finitions"),
    hr(),
    h4(HTML("<a href='https://www.dora.lib4ri.ch/wsl/islandora/object/wsl:22453/datastream/PDF/B%C3%BCtler-2020-Guide_de_poche_des_dendromicrohabitats.pdf'> 
    Guide de poche des dendromicrohabitats </a>")),
    downloadButton(outputId = ns("downloaddmh"), label = "Guide Score DMH"),
    
    hr(),
    h4("Tableau : Pourquoi conserver un arbre ?"),
    imageOutput(ns("conserv")),
    
    hr(),
    h4("Tableau : Pourquoi enlever un arbre ?"),
    imageOutput(ns("enlev")),
    
    hr(),
    h4("Sch\u00e9ma d\u00e9cisionel : Conserver ou enlever un arbre"),
    imageOutput(ns("scheme")),
    
  )
}

#' ressources Server Functions
#'
#' @noRd 
mod_ressources_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$conserv <- renderImage({
      filename <- normalizePath(file.path('inst/app/www/pictures/conserv.png'))
      list(src = filename,
           width = 651,
           height = 400)
    }, deleteFile = FALSE)
    
    output$enlev <- renderImage({
      filename <- normalizePath(file.path('inst/app/www/pictures/enlev.png'))
      list(src = filename,
           width = 651,
           height = 400)
    }, deleteFile = FALSE)
    
    output$scheme <- renderImage({
      filename <- normalizePath(file.path('inst/app/www/pictures/scheme.jpg'))
      list(src = filename,
           width = 1050,
           height = 300)
    }, deleteFile = FALSE)
    
    output$downloaddef <- downloadHandler(
      filename = function() {
        paste("definitions", ".txt", sep = "")
      },
      content = function(file) {
        file.copy("inst/app/www/definitions.txt",file)
      },
      contentType = "inst/app/www/definitions.txt"
    )
    
    output$downloaddmh <- downloadHandler(
      filename = function() {
        paste("Score_DMH", ".docx", sep = "")
      },
      content = function(file) {
        file.copy("inst/app/www/Score_DMH.docx",file)
      },
      contentType = "inst/app/www/Score_DMH.docx"
    )
    
    output$downloadNotice <- downloadHandler(
      filename = function() {
        paste("Notice_Application", ".docx", sep = "")
      },
      content = function(file) {
        file.copy("inst/app/www/Notice_Application.docx",file)
      },
      contentType = "inst/app/www/Notice_Application.docx"
    )
    
    
  })
}

## To be copied in the UI
# mod_ressources_ui("ressources_ui_1")

## To be copied in the server
# mod_ressources_server("ressources_ui_1")
