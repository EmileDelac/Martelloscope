#' qfield UI Function
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd 
#' @importFrom shiny NS tagList 
#' @import sf
#' @import ggplot2
#' @import dplyr
mod_qfield_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"),"Ajouter votre Fichier",buttonLabel="Parcourir", multiple = TRUE),
        checkboxInput(inputId = ns('header'), label = 'En-tête', value = TRUE),
        checkboxInput(inputId = ns("stringAsFactors"), "stringAsFactors", FALSE),
        radioButtons(inputId = ns('sep'), label = 'Séparateur', choices = c(Virgule=',',PointVirgule=';',Tabulation='\t', Espace=''), selected = ';'),
        radioButtons(inputId = ns('dec'), label = 'Séparateur décimal', choices = c(Virgule=',',Point='.'), selected = ','),
        hr(),
        downloadButton(outputId = ns("downloadData"), label = "Télécharger le fichier Qfield", color = '#FFE797', icon = icon("mountain")),

      ),
      mainPanel(
        strong(textOutput(ns("txtqf1"))),
        plotOutput(ns("mymap")),
        hr(),
        HTML('<p>Les participants pourront alors ouvrir le fichier .gpkg via QField <br>
                                 Ouvrir un fichier local > Stockage interne > Download</p>')
      )
    ),
  )
}

#' qfield Server Functions
#'
#' @noRd 
mod_qfield_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #Fichier Gpkg + style
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("Marteloscope", ".gpkg", sep = "")
      },
      content = function(file) {
        df <- read.table(file = input$file$datapath,
                         dec =input$dec, sep = input$sep,
                         header = input$header,
                         stringsAsFactors = input$stringAsFactors) %>% 
          mutate(mart=NA)
        df$mart <- as.factor(df$mart)
        
        style_gpkg <- sf::st_read("donnees/Iris.gpkg", layer = "layer_styles")
        style_gpkg$f_table_name <- as.factor("pts")
        
        sf::st_write(obj = sf::st_as_sf(df, coords = c("X","Y"), crs = 2154), 
                     dsn = "Marteloscope.gpkg", layer = "pts", 
                     append = TRUE, delete_layer = TRUE)
        sf::st_write(style_gpkg, "Marteloscope.gpkg", "layer_styles", append = TRUE)
      }
    )

    ##Carte à partir du csv
    output$mymap <- renderPlot({
      if(is.null(input$file)){return()}
      df <- read.table(file = input$file$datapath,
                       dec = input$dec, sep = input$sep,
                       header = input$header,
                       stringsAsFactors = input$stringAsFactors)

      ggplot(data = df, aes(x = X, y = Y, color = ess, size = dia2))  + geom_point() +
        labs(x = "X", y = "Y", fill = "Essence") + scale_color_discrete(name = "Essence") + scale_size_continuous("Diamètre") +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank(),)
    },bg ='#B1D5B3')
    
    ##Texte
    output$txtqf1 <- renderText({
      HTML('Une fois votre fichier csv chargé vous êtes invité à le télécharger ci-contre en format Qgis')
    })
  })
}

## To be copied in the UI
# mod_qfield_ui("qfield_ui_1")

## To be copied in the server
# mod_qfield_server("qfield_ui_1")
