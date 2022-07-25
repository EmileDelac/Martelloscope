#' production UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import sf
#' @import ggplot2
#' @import dplyr
mod_production_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(inputId = ns("comp"), label = strong("Fichier Martelage"), multiple = F, placeholder = "", buttonLabel = "Parcourir"),
        radioButtons(inputId = ns('sep'), label = 'S\u00e9parateur', choices = c(Virgule=',',PointVirgule=';'), selected = ';'),
        radioButtons(inputId = ns('dec'), label = 'S\u00e9parateur d\u00e9cimal', choices = c(Virgule=',',Point='.'), selected = ','),
        hr(),
        numericInput(inputId = ns("surface"), value = 1, label = "Surface en hectare", step = 0.1),
        numericInput(inputId = ns("duree"), value = 5, label = "Dur\u00e9e de rotation", step = 1),
        numericInput(inputId = ns("tarif"), value = 10, label = "Tarif Algan n° :", step = 1),

      ),
      mainPanel(
        
        tableOutput(ns("table")),
        fluidRow(
          column(6, tableOutput(ns("tabprod")),
          ),
          column(6, tableOutput(ns("tabNGV")),
          ),
        ),
        hr(),
        plotOutput(ns("nbtigedia")),
        hr(),
        plotOutput(ns("Gdia")),
      )
    ),
  )
}

#' production Server Functions
#'
#' @noRd 
mod_production_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$comp, ignoreInit = F, {
      r$tab <- read.table(file = input$comp$datapath,
                          dec =input$dec, sep = input$sep,
                          header = TRUE,
                          na.strings = c("","NA"),
                          stringsAsFactors = FALSE)
    })
    ## R\u00e9sum\u00e9 Peuplement
    output$table <- renderTable({
      req(r$tab)
      df <- r$tab
      validate(
        need(df$dia2 != "", "V\u00e9rifier les s\u00e9parateurs et recharger le .csv")
      )
      cutG <- dia2cat5cm(df$dia2)
      G <- aggregate(cutG$G~df$ess, FUN = sum)[,2]
      DF <- data.frame(cbind (table(df$ess,cutG$cut), Total = rowSums(table(df$ess,cutG$cut)), G))
      DF <- round(DF)  
      colnames(DF) <- c("15","20","25","30","35","40","45","50","55","60","65","70","75","80","85","90","...","Total","G")
      rownames(DF) <- levels(as.factor(df$ess))
      DF
    },digits = 0, rownames = TRUE)
    
    #Passage futaie et Accroissement
    
    output$tabprod <- renderTable({
      req(r$tab)
      r$tabessence <- traitement_essence(r$tab, duree = input$duree, surface = input$surface)
      r$tabessence
    })
    
    # N G V
    
    output$tabNGV <- renderTable({
      req(r$tab)
      df <- r$tab
      validate(
        need(df$dia2 != "", "")
      )
      df <- df[!(is.na(df$dia2) | df$dia2 == 0),]
      N <- length(df$dia2)/input$surface
      G <- sum(dia2cat5cm(df$dia2)$G)/input$surface
      V <- sum(perrotte(df$dia2/100,input$tarif))/input$surface
      Nt <- c("N :", N, "tiges/ha")
      Gt <- c("G :", round(G), "m²/ha")
      Vt <- c("V :", round(V), "m^3/ha")
      tabt <- data.frame(rbind(Nt,Gt,Vt))
      colnames(tabt) <- c("","","")
      tabt
      
    })
    
    #Graphique Nombre de tige par classe de diamètre
    output$nbtigedia <- renderPlot({
      req(r$tab)
      df <- r$tab
      validate(
        need(df$dia2 != "", "V\u00e9rifier les s\u00e9parateurs et recharger le .csv")
      )
      df <- r$tab %>% mutate(cat = cut(as.numeric(dia2), breaks = c(17.5, 27.5, 42.5, 62.5, 2000), labels = c("PB", "BM", "GB", "TGB")))
      df <- df[!is.na(df$cat),]
      ggplot(data = df, aes(x = cat, fill = as.factor(ess)))  + geom_bar(width = .5) + labs(x = "Classes de diam\u00e8tre", y = "Nombre de tiges", fill = "Essence")+ ggtitle("Nombre de tiges par cat\u00e9gorie de diam\u00e9tre") + scale_color_discrete(name = "Essence") 
      
    },bg ='#B1D5B3')
    
    #Graphique surface terrière par classe de diamètre
    output$Gdia <- renderPlot({
      if(is.null(input$comp)){return()}
      df <- r$tab
      validate(
        need(df$dia2 != "", "V\u00e9rifier les s\u00e9parateurs et recharger le .csv")
      )
      df <- r$tab %>% mutate(g = (dia2/100)^2*pi/4/input$surface) %>% mutate(cat = cut(as.numeric(dia2), breaks = c(17.5, 27.5, 42.5, 62.5, 2000), labels = c("PB", "BM", "GB", "TGB")))
      df <- df[!is.na(df$cat),]
      ggplot(data = df, aes(x = cat, y = g))  + geom_col(fill = '#7895A2', width = .5) + labs(x = "Classes de diam\u00e9tre", y = "surface terri\u00e9re")+ ggtitle("Surface terri\u00e9re par cat\u00e9gorie de diam\u00e9tre")
    },bg ='#B1D5B3')
  })
}

## To be copied in the UI
# mod_production_ui("production_ui_1")

## To be copied in the server
# mod_production_server("production_ui_1")
