#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(DT)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  navbarPage("",
    tabPanel("Mapas de SD",
      titlePanel("Mapas dinámicos en R y shiny"),
      sidebarLayout(
        sidebarPanel(
          # Select party
          selectInput("partido",
                      "Partido:",
                      choices = list("PRI", "PAN", "PRD"), 
                      selected = 1
          ),
          # Select year
          sliderInput("year",
                     "Año:",
                     min = 2000,
                     max = 2012,
                     value = 2000,
                     step = 6,
                     sep = "",
                     animate=TRUE
          ), width = 3
        ),
        mainPanel(
          # Show  SD plots
          plotOutput("sdPlot")
        )
      )
    ),
    tabPanel("Mapas de vecindades",
      titlePanel("Vecindades en R y shiny"),
      sidebarLayout(
        sidebarPanel(
          # Select state coming from server
          uiOutput("distritoSelect"), width = 3
        ),
        mainPanel(
          textOutput("text1"),
          plotOutput("nbPlot")
        )
      )
    ),
    tabPanel("Vecindades dinámicas",
     titlePanel("Vecindades dinámicas en Leaflet"),
       # mainPanel(
         #textOutput("clicked"),
         leafletOutput("NBmap")
       # )
    ),
    tabPanel("LISA dinámica",
      titlePanel("Cálculo de LISA y otras cosas dinámicas"),
      sidebarLayout(
        sidebarPanel(
          # Select stuff to work with
          radioButtons('radio', label = 'Selecciona el tipo de vecindad:',  
                       choices = list("Tipo reina (queen)" = 1, "Tipo torre (rook)" = 2, 
                                      'Vecinos mas cercanos (kNN)' = 3, 'Distancia' = 4), 
                       selected = 1), 
          conditionalPanel(
            condition = "input.radio == 3", 
            sliderInput("knn_slider", 'Selecciona el número de vecinos', 
                        min = 1, max = 32, value = 4)
          ), 
          conditionalPanel(
            condition = "input.radio == 4", 
            sliderInput("dist_slider", "Seleciona un umbral de distancia en km", 
                        min = 450, max = 3000, step = 10, value = 450)
          ),
          # Select party
          selectInput("partidoLISA",
                      "Partido:",
                      choices = list("PRI", "PAN", "PRD"), 
                      selected = 1
          ),
          # Select year
          sliderInput("yearLISA",
                      "Año:",
                      min = 2000,
                      max = 2012,
                      value = 2000,
                      step = 6,
                      sep = "",
                      animate=TRUE
          )
        ),
        mainPanel(
          plotOutput("moranPlot", height = 400, brush = brushOpts(id = "plot_brush")),
          DT::dataTableOutput("table", width = '100%'),
          leafletOutput("LISAmap", width = '100%')
        )
      )
    )
  )
))
