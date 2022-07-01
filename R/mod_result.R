#' result UI Function
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd 
#' @import sf ggplot2 dplyr rmarkdown knitr
#' @importFrom shiny NS tagList 
mod_result_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        numericInput(inputId = ns("surface"), value = 1.5, label = "Surface en hectare", step = 0.1),
        numericInput(inputId = ns("duree"), value = 5, label = "Dur\u00e9e de rotation", step = 1),
        numericInput(inputId = ns("tarif"), value = 10, label = "Tarif Algan n° :", step = 1),
        hr(),
        h3('Donn\u00e9es optionnelles'),
        fileInput(ns("parcel"), label = "fichiers .shp de la parcelle", buttonLabel = "Parcourir", accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE),
        fileInput(ns("houp"), label = "fichiers .shp des houppiers", buttonLabel = "Parcourir", accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE),
        hr(),
        numericInput(ns("quali_A"), label = 'Prix de la qualit\u00e9 A', value = 0),
        numericInput(ns("quali_B"), label = 'Prix de la qualit\u00e9 B', value = 0),
        numericInput(ns("quali_C"), label = 'Prix de la qualit\u00e9 C', value = 0),
        numericInput(ns("quali_D"), label = 'Prix de la qualit\u00e9 D', value = 0)
      ),
      mainPanel(
        textOutput(ns("MDSpath")),
        hr(),
        fluidRow(
          column(3,
          ),
          column(6,
                 fileInput(inputId = ns("martgpkg"), label = strong("Fichiers Martelages .gpkg"), multiple = T, placeholder = "", buttonLabel = "Parcourir"),
          ),
        ),
        fluidRow(
          column(3,),
          column(6,
                 downloadButton(ns("report"),"G\u00e9n\u00e9rer le rapport"),
          ),
        ),
        hr(),
        fluidRow(
          column(5, downloadButton(ns("nontitre"),"G\u00e9n\u00e9rer le rapport individuel")),
          column(2,"de :"),
          column(5, selectInput(ns("nomart"),label = NULL,"")),
        ),
        hr(),
        fluidRow(
          downloadButton(ns("listing"),"Listing des arbres martel\u00e9s"),
        ),
        fluidRow(
          column(6,
                 plotOutput(ns("maparcel"))
          ),
          column(6,
                 plotOutput(ns("meshoup"))
          ),
        ),
      )
    ),
  )
}

#' result Server Functions
#'
#' @noRd 
mod_result_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$MDSpath <- renderText(
      HTML("Une fois les martelages effectu\u00e9s ils pourront être transf\u00e9r\u00e9s depuis le dossier 
          t\u00e9l\u00e9chargement des MDS (fichier : .gpkg) vers l'ordinateur et renomm\u00e9s pour être ajout\u00e9s ci-dessous :" )
    )
    
    #________________________________________________________  Prix  _____________________________________________________
    
    tab_prix <- reactive({
      quali <- c("A","B","C","D")
      prix_quali <- c(input$quali_A,input$quali_B,input$quali_C,input$quali_D)
      tab_prix <- data.frame(quali,prix_quali)
    })
    
    
    
    #______________________________  Rapport Individuel  ___________________________________
    
    observe({
      x <- input$martgpkg
      if (is.null(x))
        x <- character(0)
      
      updateSelectInput(session, 
                        "nomart",
                        choices = x
      )
    })
    
    
    output$nontitre <- downloadHandler(
      filename = function() {
        paste("nontitre", ".html", sep = "")
      },
      content = function(file2) {
        
        file.copy(rmarkdown::render(knit("R/nontitre.Rmd")),to = file2)
      },
    )
    #______________________________  Listing des arbres  ___________________________________
    
    
    output$listing <- downloadHandler(
      filename = function() {
        paste("listing", ".html", sep = "")
      },
      content = function(file3) {
        
        file.copy(rmarkdown::render(knit("R/listing.Rmd")),to = file3)
      },
    )
    
    #____________________________________________ Lecture de fichier GPKG _______________________________________  
    
    datagpkg <- reactive({
      liste <- list()
      for (i in 1:length(input$martgpkg[,1])){
        liste[[i]] <-  st_read( input$martgpkg[[i, 'datapath']],layer = st_layers(input$martgpkg[[i, 'datapath']])$name[[1]])
      }
      # Nom des fichiers upload\u00e9s
      if (!is.null(input$martgpkg$datapath)) {
        # Extract file name (additionally remove file extension using sub)
        nomartgpkg <- (sub(".gpkg$", "", basename(input$martgpkg$name)))
      }
      liste2 <- list()
      liste3 <- list()
      for(i in 1:length(input$martgpkg[,1])){
        nom <- rep(nomartgpkg[i],dim(as.data.frame(liste[i]))[1])
        liste2[[i]] <- cbind(as.data.frame(liste[i]),nom)}
      liste3 <- bind_rows(liste2)
      
    })
    
    
    
    
    #________________________________________________________  Rapport  _____________________________________________________
    
    output$report <- downloadHandler(
      filename = function () {
        paste0("report",".",'docx')
      },
      content = function(file){
        showModal(modalDialog("Chargement", footer=NULL, easyClose = TRUE, fade = TRUE))
        report_path <- tempfile(fileext = ".Rmd")
        file.copy("report.Rmd", report_path,  overwrite = TRUE)
        
        
        rmarkdown::render(report_path,
                          output_format =  rmarkdown::word_document(),
                          output_file = file)
        on.exit(removeModal())
      }
    )
    
    #____________________________________________ Lecture de fichiers SHP _______________________________________ 
    
    #______________________Parcelle
    sfparcel <- reactive({
      shpdf <- input$parcel
      if(is.null(shpdf)){
        return()
      }
      previouswd <- getwd()
      uploaddirectory <- dirname(shpdf$datapath[1])
      setwd(uploaddirectory)
      for(i in 1:nrow(shpdf)){
        file.rename(shpdf$datapath[i], shpdf$name[i])
      }
      setwd(previouswd)
      sfparcel <- sf::read_sf(paste(uploaddirectory, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/"))#,  delete_null_obj=TRUE)
    })
    #______________________Houppiers
    sfhoup <- reactive({
      shpdf <- input$houp
      if(is.null(shpdf)){
        return()
      }
      previouswd <- getwd()
      uploaddirectory <- dirname(shpdf$datapath[1])
      setwd(uploaddirectory)
      for(i in 1:nrow(shpdf)){
        file.rename(shpdf$datapath[i], shpdf$name[i])
      }
      setwd(previouswd)
      sfhoup <- sf::read_sf(paste(uploaddirectory, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/"))#,  delete_null_obj=TRUE)
    })
    
    #____________________________________________ Affichage parcelle / houppiers _______________________________________ 
    
    output$maparcel <-renderPlot({
      if(!is.null(input$parcel)){
        emprise <- sfparcel()
        return(
          ggplot() + geom_sf(data = emprise)
        )}
    },bg ='#B1D5B3')
    
    output$meshoup <-renderPlot({
      
      if(!is.null(input$houp)){
        canop <- sfhoup()
        return(
          ggplot()  + geom_sf(data = canop)
        )}
    },bg ='#B1D5B3')
    
    
    
  })
}

## To be copied in the UI
# mod_result_ui("result_ui_1")

## To be copied in the server
# mod_result_server("result_ui_1")
