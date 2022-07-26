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
        radioButtons(inputId = ns('sep'), label = 'S\u00e9parateur', choices = c(Virgule=',',PointVirgule=';'), selected = ';'),
        radioButtons(inputId = ns('dec'), label = 'S\u00e9parateur d\u00e9cimal', choices = c(Virgule=',',Point='.'), selected = ','),
        hr(),
        downloadButton(outputId = ns("downloadData"), label = "T\u00e9l\u00e9charger le fichier Qfield", color = '#FFE797', icon = icon("mountain")),

      ),
      mainPanel(
        strong(textOutput(ns("txtqf1"))),
        plotOutput(ns("mymap")),
        hr(),
        HTML('<p>Les participants pourront alors ouvrir le fichier .gpkg via QField <br>
                                 Ouvrir un fichier local \u003E Stockage interne \u003E Download</p>')
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
        paste0("Marteloscope", ".zip", sep = "")
      },
      content = function(fileqf) {
        temp_gpkg <- tempdir()
       
          df <- read.table(file = input$file$datapath,
                                   dec =input$dec, sep = input$sep,
                           header = TRUE,
                           na.strings = c("","NA"),
                           stringsAsFactors = FALSE) %>% 
                    mutate(mart=NA)
                  df$mart <- as.factor(df$mart)
                  
                  style_gpkg <- sf::st_read("Iris.gpkg", layer = "layer_styles")
                  style_gpkg$f_table_name <- as.factor("pts")
                  
                  df2 <- sf::st_write(obj = sf::st_as_sf(df, coords = c("X","Y"), crs = 2154), 
                               dsn = "Marteloscope.gpkg", layer = "pts", 
                               append = TRUE, delete_layer = TRUE)
                  sf::st_write(style_gpkg, "Marteloscope.gpkg", "layer_styles", append = TRUE)
                  
                  zip_file <- file.path(temp_gpkg, "Marteloscope.zip")
                  gpkg_files <- list.files(temp_gpkg,
                                           "Marteloscope",
                                          full.names = TRUE)
                  zip_command <- paste("zip -j", 
                                       zip_file, 
                                       paste(gpkg_files, collapse = " "))
                  system(zip_command)
                  # copy the zip file to the file argument
                  file.copy(zip_file, fileqf)
                  # remove all the files created
                  file.remove(zip_file, gpkg_files)
      }
    )

    ##Carte a partir du csv
    output$mymap <- renderPlot({
      if(is.null(input$file)){return()}
      df <- read.table(file = input$file$datapath,
                       dec = input$dec, sep = input$sep,
                       header = TRUE,
                       na.strings = c("","NA"),
                       stringsAsFactors = FALSE)
      validate(
        need(df$X != "", "V\u00e9rifier les s\u00e9parateurs et recharger le .csv")
      )

      ggplot(data = df, aes(x = X, y = Y, color = ess, size = dia2))  + geom_point() +
        labs(x = "X", y = "Y", fill = "Essence") + scale_color_discrete(name = "Essence") + scale_size_continuous("Diametre") +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank(),)
    },bg ='#B1D5B3')
    
    ##Texte
    output$txtqf1 <- renderText({
      HTML('Une fois votre fichier csv charg\u00e9 vous \u00eates invit\u00e9 \u00e0 le t\u00e9l\u00e9charger ci-contre en format Qgis')
    })
  })
}

## To be copied in the UI
# mod_qfield_ui("qfield_ui_1")

## To be copied in the server
# mod_qfield_server("qfield_ui_1")
