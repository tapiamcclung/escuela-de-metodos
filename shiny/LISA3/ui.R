#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# Rodrigo Tapia-McClung (2017)

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  navbarPage("",
    tabPanel("Mapas de SD",
      titlePanel("Mapas din??micos en R y shiny"),
      sidebarLayout(
        sidebarPanel(
          # Select year
          sliderInput("year",
                      "A??o:",
                      min = 2006,
                      max = 2012,
                      value = 2006,
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
           uiOutput("estadosSelect"), width = 3
         ),
        mainPanel(
          textOutput("text1"),
          plotOutput("nbPlot")
        )
      )
    ),
    tabPanel("Vecindades din??micas",
      titlePanel("Vecindades din??micas en Leaflet"),
        #mainPanel(
        #textOutput("clicked"),
        leafletOutput("NBmap")
      #)
    )
  )
  
))
