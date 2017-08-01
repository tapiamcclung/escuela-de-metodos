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
library(GISTools)
library(RColorBrewer)
library(leaflet)

# Leer shp de estados
edos <- readOGR("../data", "estados_sorted", verbose = FALSE, stringsAsFactors = FALSE, GDAL1_integer64_policy = T)
# Leer csv de datos
homicidios <- read.csv("../data/homicidios.csv", colClasses = c(rep("character", 1), rep("integer", 7)))
# Hacer el join del shp con los datos
edos <- merge(edos, homicidios, by.x = "CVE_ENT")

# Los vecinos de todos los estados QUEEN
edos.nbq <- poly2nb(edos, queen = T)#, row.names = as.character(edos$ADMIN_NAME))
edos.nbq.w <- nb2listw(edos.nbq)
# Las coordenadas de los centroides de los estados
coords <- coordinates(edos)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  ############################
  # TAB: Dynamic NB map
  ############################
  
  output$NBmap <- renderLeaflet({
    NBmap <- leaflet(edos) %>%
      addProviderTiles('CartoDB.Positron') %>%
      addPolygons(layerId = ~CVE_ENT, fillColor = 'yellow', #popup = popup,
                  color = 'blue', weight = 0.5, smoothFactor = 0.1)
    NBmap
  })
  
  click_edo <- eventReactive(input$NBmap_shape_click, {
    return(input$NBmap_shape_click$id)
  })
  
  output$clicked <- renderText({ 
    click_edo()
  })
  
  focal_edo <- reactive({
    req(click_edo())
    return(edos[edos$CVE_ENT == click_edo(), ])
  })
  
  #Define neighbors for clicked polygon
  neighbors <- reactive({
    req(click_edo())
    # Los estados vecinos al seleccionado
    return(edos[edos$CVE_ENT %in% edos.nbq[[click_edo()]], ])
  })
  
  # Add nb polys
  observe({
    req(click_edo())
    proxy <- leafletProxy('NBmap')
    if (length(neighbors()) > 0){
      if (!is.null(neighbors())) {
        proxy %>%
          removeShape('focal') %>%
          clearGroup('neighbors') %>%
          addPolygons(data = neighbors(), fill = FALSE, color = 'skyblue',
                      group = 'neighbors', opacity = 1) %>%
          addPolygons(data = focal_edo(), color = 'tomato', 
                      opacity = 1, layerId = 'focal', fillColor = 'transparent')
      } else {
        proxy %>%
          removeShape('focal') %>%
          clearGroup('neighbors') %>%
          addPolygons(data = focal_edo(), color = 'tomato', 
                      opacity = 1, layerId = 'focal', fillColor = 'transparent')
      }
    } else {
      proxy %>%
        removeShape('focal') %>%
        clearGroup('neighbors')
    }
  })
  
})
