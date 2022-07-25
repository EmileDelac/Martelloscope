#' accueil UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_accueil_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    h4(align = "center", "Bienvenu sur cet outil d'aide \u00e0 la mise en place d'un Marteloscope"),
    hr(),
    HTML("<p> <ul>Pour utiliser cet outil vous aurez besoin : 
                                <li> de l'application <a href='https://play.google.com/store/apps/details?id=ch.opengis.qfield&hl=fr&gl=US'>Qfield</a> sur t\u00e9l\u00e9phone mobile ou tablette </li> 
                                <li> d'un fichier <a href='https://www.youtube.com/watch?v=e_-YYRuFDA4&ab_channel=BrunoRoche'>(.csv)</a> contenant la localisation des arbres de votre marteloscope  </li></ul></p>"),
    
    HTML("<p></p>
                      <p><ul>Ce fichier doit comporter (comme dans l'exemple ci-dessous) : 
                      <li>une colonne 'X' et 'Y' (en majuscule) pour la localisation</li>
                      <li>une colonne 'ess' pour l'essence de chaque arbre</li>
                      <li>une ou deux colonnes pour le diam\u00e8tre : dia1 ; dia2</li></ul></p>"),
    actionButton(
      inputId = ns( "success"),
      label = "dia1 ; dia2 ?",
    ),
    
    div(tableOutput(ns("tabintro")), align="center"),
    
    hr(),
    HTML("<p></p>
                      <p><ul>Ce fichier peut aussi comporter : 
                      <li>une colonne 'dmh' (note sur 10) des dendro-micro habitats</li>
                      <li>une colonne 'qualite' ('A','B','C','D') </li></ul></p>"),
    hr(),
    fluidRow(
      column(3, align= "center",strong("Caract\u00e9risation")),
      column(3, align= "center",strong("Qfield")),
      column(3, align= "center",strong("R\u00e9sultats")),
      column(3, align= "center",strong("Ressources")),
    ),
    fluidRow(
      column(3, align = "center","Cet onglet permet d'\u00e9tudier la dynamique du marteloscope dans le temps, en d\u00e9terminant le passage \u00e0 la futaie et l'accroissement"),
      column(3, align = "center","Cet onglet permet de cr\u00e9er le fichier terrain \u00e0 transmettre aux participants"),
      column(3, align = "center","Cet onglet permet de comparer les martelages des participants"),
      column(3, align = "center","Cet onglet regroupe un lexique et des documents d'aide \u00e0 la d\u00e9cision pour les participants"),
      
    ),
  )
}

#' accueil Server Functions
#'
#' @noRd 
mod_accueil_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$tabintro <- renderTable({
      r1 <- c("1","Sapin","30","35", 73.8522, 822.54838,"...")
      r2 <- c("2","Epicea","22","26", 73.8369, 822.54844,"...")
      r3 <- c("3","Sapin","52","56", 73.8102, 822.54783,"...")
      nom <- c("num","ess","dia1","dia2","X","Y","...")
      dat <- rbind(r1,r2,r3)
      colnames(dat) <- nom
      dat
    })
    observeEvent(input$success, {
      shinyalert::shinyalert(
        title = "dia1 & dia2",
        text = "'dia2' doit correspondre au dernier diam\u00e8tre mesur\u00e9 ;
        'dia1' correspond au diam\u00e8tre mesur\u00e9 il y a 'n' ann\u00e9es avec n = Dur\u00e9e de rotation (onglet R\u00e9sultats)
        Si vous ne disposez que d'une valeur de diam\u00e8tre, veuillez nommer la colonne 'dia2'",)
    })
    
  })
}

## To be copied in the UI
# mod_accueil_ui("accueil_ui_1")

## To be copied in the server
# mod_accueil_server("accueil_ui_1")
