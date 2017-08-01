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
library(sp)
library(spdep)
library(ggplot2)
library(GISTools)
library(RColorBrewer)

# Leer shp de estados
edos <- readOGR("../data", "estados_sorted", verbose = FALSE, stringsAsFactors = FALSE, GDAL1_integer64_policy = T)
# Leer csv de datos
homicidios <- read.csv("../data/homicidios.csv", colClasses = c(rep("character", 1), rep("integer", 7)))
# Hacer el join del shp con los datos
edos <- merge(edos, homicidios, by.x = "CVE_ENT")

# Los vecinos de todos los estados QUEEN
edos.nbq <- poly2nb(edos, queen = T)#, row.names = as.character(edos$ADMIN_NAME))
edos.nbq.w <- nb2listw(edos.nbq)
# Los vecinos de todos los estados ROOK
edos.nbr <- poly2nb(edos, queen = F)#, row.names = as.character(edos$ADMIN_NAME))
edos.nbr.w <- nb2listw(edos.nbr)
# Las coordenadas de los centroides de los estados
coords <- coordinates(edos)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  year <- reactive({ 
    as.character(input$year) 
  })
  
  ############################
  # TAB 1: SD plots
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
  
  ############################
  # TAB 2: NB plots
  ############################
  
  output$estadosSelect <- renderUI({
    selectInput("estado",
                "Estado:",
                choices = as.list(unique(edos$ADMIN_NAME)),
                selected = "Aguascalientes")
  })
  
  # El estado seleccionado en el menu
  estado <- reactive({
    if (is.null(input$estado)) {
      return(NULL)
    }
    input$estado
  })
  
  # neighbors plot
  output$nbPlot <- renderPlot({
    if (is.null(estado())) {
      return()
    }
    # El estado seleccionado
    sel.state <- subset(edos, ADMIN_NAME==estado())
    # ID del estado seleccionado
    id <- as.integer(sel.state$CVE_ENT)
    # Los estados vecinos al seleccionado
    subset.edos <- edos[edos$CVE_ENT %in% edos.nbq[[id]], ]
    
    # TODO: not sure if its ok to make outputs inside other outpus...
    output$text1 <- renderText({
      if (length(subset.edos) > 1) vecinos <- "vecinos" else vecinos <- "vecino"
      paste(estado(), "tiene", length(subset.edos), vecinos, length(edos.nbq[[id]]))
    })
    
    # Juntar los polígonos que nos interesan
    relevantPolys <- rbind(sel.state, subset.edos)
    
    #subset.nb2 <- subset(edos.nbq, (1:length(edos.nbq) %in% as.integer(relevantPolys$CVE_ENT)))
    # Calcular los vecinos del poligono seleccionado
    subset.nb <- poly2nb(relevantPolys, queen = T)  # TRUE: tipo reina
    # Calcular sus centroides
    coords2 <- coordinates(relevantPolys)
    
    # Graficar el país y vecinos adyacentes del estado seleccionado
    op <- par(
      oma = c(0,0,0,0),# Sin margen arriba
      mar = c(0,0,0,0)) # Sin margen entre plots
    par(bg = '#a5bfdd')
    
    plot(edos, col = '#f1f4c7')
    # Pintar los estados que interesan
    plot(relevantPolys, col = "skyblue", add= T)
    # Destacar el seleccionado
    plot(sel.state, col = "tomato", add= T)
    # Pintar todos los vecinos
    plot(edos.nbq, coords = coords, pch = 19, cex = 0.1, col = "red", add = T)
    # Pintar las conexiones del estado seleccionado
    plot(subset.nb, coords = coords2, pch = 19, lwd = 2, cex = 0.1, col = "blue", add = T)
    #plot(subset.nb2, coords = coords2, pch = 19, cex = 0.1, col = "green", add = T)
    
  })
  
})
