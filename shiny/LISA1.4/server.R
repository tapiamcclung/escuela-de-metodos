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
library(leaflet)
library(DT)
library(dplyr) # load this last so select is not masked by MASS (loaded by RColorBrewer)


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


# Define server logic required to draw stuff
shinyServer(function(input, output) {
  
  ############################
  # TAB: Dynamic LISA
  ############################
  
  yearLISA <- reactive({ 
    as.character(input$yearLISA) 
  })

  selected <- reactive({
    var <- switch(yearLISA(), 
                 "2006" = edos$A2006,
                 "2007" = edos$A2007,
                 "2008" = edos$A2008,
                 "2009" = edos$A2009,
                 "2010" = edos$A2010,
                 "2011" = edos$A2011,
                 "2012" = edos$A2012)
    
    # Distance-based spatial weights
    knn <- reactive({
      if (input$radio == 3) {
        k <- knearneigh(coordinates(edos), k = input$knn_slider, longlat = TRUE)
        return(k$nn)
      } else {
        return(NULL)
      }
    })
    
    dist <- reactive({
      if (input$radio == 4) {
        return(dnearneigh(coordinates(edos), 0, input$dist_slider, longlat = TRUE))
      } else {
        return(NULL)
      }
    })
    
    # LISA Map
    edo_weights <- reactive({
      if (input$radio == 1) {
        #return(nb2listw(include.self(poly2nb(edos))))
        return(nb2listw(poly2nb(edos)))
      } else if (input$radio == 2) {
        #return(nb2listw(include.self(poly2nb(edos, queen = FALSE))))
        return(nb2listw(poly2nb(edos, queen = FALSE)))
      } else if (input$radio == 3) {
        k <- knearneigh(coordinates(edos), k = input$knn_slider, longlat = TRUE)
        #return(nb2listw(include.self(knn2nb(k))))
        return(nb2listw(knn2nb(k)))
      } else if (input$radio == 4) {
        d <- dnearneigh(coordinates(edos), 0, input$dist_slider, longlat = TRUE)
        #return(nb2listw(include.self(d)))
        return(nb2listw(d))
      }
    })
    
    edos$var <- var
    edos$var_lag <- lag.listw(edo_weights(), var)
    
    varStd <- (var - mean(var))/sd(var)
    lagStd <- lag.listw(edo_weights(), scale(var))
    
    mean <- mean(var)
    lag_mean <- mean(edos$var_lag) 
    
    global_moran <- moran.test(var, edo_weights())
    statistic <- (global_moran$estimate)
    statistic <- round(statistic, 2)
    lisa <- localmoran(var, edo_weights())
    
    edos$cuadrante <- c(rep(0,length(var)))
    significance <- 0.05
    vec <- ifelse(lisa[,5] < significance, 1,0)
    
    edos$cuadrante[var >= mean & edos$var_lag >= lag_mean]  <- 1
    edos$cuadrante[var < mean & edos$var_lag < lag_mean]  <- 2
    edos$cuadrante[var < mean & edos$var_lag >= lag_mean]  <- 3
    edos$cuadrante[var >= mean & edos$var_lag < lag_mean]  <- 4
    edos$cuadrante.data <- edos$cuadrante*vec
    
    edos$cuadrante.col[edos$cuadrante.data==1] <- "High-High"
    edos$cuadrante.col[edos$cuadrante.data==2] <- "Low-Low"
    edos$cuadrante.col[edos$cuadrante.data==3] <- "Low-High"
    edos$cuadrante.col[edos$cuadrante.data==4] <- "High-Low"
    edos$cuadrante.col[edos$cuadrante.data==0] <- "Non-sig"
    
    edos$fill <- factor(edos$cuadrante.data+1)
    edos$var_mean <- mean
    edos$var_lag_mean <- lag_mean
    edos$statistic <- statistic[1]
    
    edos.sel <- subset(edos, select = c(ADMIN_NAME, CVE_ENT, var, var_lag, cuadrante, cuadrante.data,
                                        cuadrante.col, fill, var_mean, var_lag_mean, statistic))
  })
  
  output$moranPlot <- renderPlot({
    # cat(file=stderr(), "var", selected()@data$var,  "\n")
    # cat(file=stderr(), "var_lag", selected()@data$var_lag,  "\n")
    cColors <- c(rgb(0.74, 0.74, 0.74, alpha = 0.2), rgb(1, 0, 0, alpha = 0.75),
                 rgb(0, 0, 1, alpha = 0.75), rgb(0.58, 0.58, 1, alpha = 0.75), rgb(1, 0.58, 0.58, alpha = 0.75))
    ggplot(selected()@data, aes(x=var, y=var_lag)) +
      geom_point(aes(fill = selected()$fill), colour="black", size = 3, shape = 21)+
      scale_fill_manual(name="",
                        values = c("1" = cColors[1], "2" = cColors[2], "3" = cColors[3], "4" = cColors[4], "5" =cColors[5]),
                        labels=c("Non-sig",
                                   paste0("High-High (", sum(selected()$cuadrante.data==1), ")"),  
                                 paste0("Low-Low (", sum(selected()$cuadrante.data==2), ")"),
                                 paste0("Low-High (", sum(selected()$cuadrante.data==3), ")"),
                                 paste0("High-Low (", sum(selected()$cuadrante.data==4), ")"))) +
      geom_vline(xintercept = unique(selected()$var_mean), colour = "grey", linetype = "longdash") +
      geom_hline(yintercept = unique(selected()$var_lag_mean), colour = "grey", linetype = "longdash") +
      stat_smooth(method="lm", se=FALSE, colour = "black", size = 0.5) +
      xlab("\nNúmero de homicidios por estado") +
      ylab("\nRetraso espacial de homicidios por estado") +
      theme_bw() +
      ggtitle(paste0("I de Moran: ", unique(selected()$statistic),"\n")) +
      theme(plot.title = element_text(color = "darkorchid")) 
  })
  
  output$LISAmap <- renderLeaflet({
    cColors <- c(rgb(0.74, 0.74, 0.74), rgb(1, 0, 0),
                 rgb(0, 0, 1), rgb(0.58, 0.58, 1), rgb(1, 0.58, 0.58))
    factpal <- colorFactor(cColors, 
                           domain = c("0", "1", "2", "3", "4"))
    
    popup <- paste(selected()$ADMIN_NAME,":", selected()$var, "homicidio(s) en", input$yearLISA)
    leaflet(edos) %>%
      addProviderTiles('CartoDB.Positron') %>%
       #addPolygons(layerId = ~CVE_ENT, fillColor = 'yellow',
        #           color = 'blue', weight = 0.5, smoothFactor = 0.1) %>%
      addPolygons(data = selected(), fillColor = ~factpal(cuadrante.data), 
                  fillOpacity = 0.7, color = "black", weight = 1, label = popup) %>%
      addLegend(position = "topright", colors = cColors,
                labels = c("Non-sig", "High-High", "Low-Low", "Low-High", "High-Low"), opacity = 0.7)
  })
  
  brushed <- eventReactive(input$plot_brush, {
    brushedPoints(selected(), input$plot_brush)
    #TODO: detect when there is no brush (after clicking on map) and update with empty df to avoid warning
  })
  
  output$table <- DT::renderDataTable({
    tbl <- brushed() %>%   
      as.data.frame() %>% 
      select(Estado = ADMIN_NAME, Homicidios = var, Tipo = cuadrante.col)
  },
  rownames = FALSE, options = list(pageLength = 5, dom = 'tip', autoWidth = FALSE))
  # TODO: Hide tables's div when nothing is selected... lots of white space is left in there
  
  observe({
    req(brushed())
    popup <- paste(brushed()$ADMIN_NAME, ":", brushed()$var, "homicidio(s) en", input$yearLISA)
    leafletProxy('LISAmap') %>%
    clearGroup(group = 'brushed') %>%
    addPolygons(data = brushed(), fill = "#9cf", color = '#036', weight = 1.5,
                opacity = 0.5, group = 'brushed', label = popup)
  })
  
})