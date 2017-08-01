#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  navbarPage("",
    tabPanel("Mapas de SD",
      titlePanel("Mapas dinámicos en R y shiny"),
      sidebarLayout(
        sidebarPanel(
          # Select year
          sliderInput("year",
                      "Año:",
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
    )
  )
  
))
