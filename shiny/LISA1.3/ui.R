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

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  navbarPage("",
    tabPanel("Vecindades dinámicas",
      titlePanel("Vecindades dinámicas en Leaflet"),
        #mainPanel(
        #textOutput("clicked"),
        leafletOutput("NBmap")
      #)
    )
  )
  
))
