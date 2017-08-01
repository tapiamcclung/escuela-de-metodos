#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(rgdal)
library(GISTools)
library(RColorBrewer)

# Leer shp de estados
edos <- readOGR("../data", "estados_sorted", verbose = FALSE, stringsAsFactors = FALSE, GDAL1_integer64_policy = T)
# Leer csv de datos
homicidios <- read.csv("../data/homicidios.csv", colClasses = c(rep("character", 1), rep("integer", 7)))
# Hacer el join del shp con los datos
edos <- merge(edos, homicidios, by.x = "CVE_ENT")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$estadosSelect <- renderUI({
    selectInput("estado",
                "Estado:",
                choices = as.list(unique(edos$ADMIN_NAME)),
                selected = "Aguascalientes")
  })
  
  year <- reactive({ 
    as.character(input$year) 
  })
  
  ############################
  # TAB: SD plots
  ############################
  
  output$sdPlot <- renderPlot({
    data <- switch(year(), 
                   "2006" = edos$A2006,
                   "2007" = edos$A2007,
                   "2008" = edos$A2008,
                   "2009" = edos$A2009,
                   "2010" = edos$A2010,
                   "2011" = edos$A2011,
                   "2012" = edos$A2012)
    
    par(mar = c(0, 0, 1.5, 0))
    shades <- auto.shading(data, cutter = sdCuts, n = 6, cols = rev(brewer.pal(6, "RdYlBu")))
    choropleth(edos, data, shades)
    choro.legend(-95, 32, shades, under = "<", over = ">", between = "a", box.lty = "blank", x.intersp = 0.5, y.intersp = 0.75)
    title(main = paste("Homicidios por estado en", year(),"\n(desviaciones estándar)"), cex.main = 0.75)
  })
  
})
