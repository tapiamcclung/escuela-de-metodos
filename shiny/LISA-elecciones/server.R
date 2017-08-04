#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# Rodrigo Tapia-McClung (2017)

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


# Leer shp de distritos
distritos <- readOGR("../data", "elecciones", verbose = FALSE, stringsAsFactors = FALSE, GDAL1_integer64_policy = T)
# add ID column
distritos$id <- seq.int(nrow(edos))

# Los vecinos de todos los distritos QUEEN
distritos.nbq <- poly2nb(distritos, queen = T)#, row.names = as.character(edos$ADMIN_NAME))
distritos.nbq.w <- nb2listw(distritos.nbq)
# Los vecinos de todos los distritos ROOK
distritos.nbr <- poly2nb(distritos, queen = F)#, row.names = as.character(edos$ADMIN_NAME))
distritos.nbr.w <- nb2listw(distritos.nbr)
# Las coordenadas de los centroides de los distritos
coords <- coordinates(distritos)


# Define server logic required to draw stuff
shinyServer(function(input, output) {
  
  partido <- reactive({ 
    as.character(input$partido) 
  })
  
  year <- reactive({ 
    as.character(input$year) 
  })
  
  ############################
  # TAB 1: SD plots
  ############################
  
  output$sdPlot <- renderPlot({
    # if (partido() == 'PRI' && year() == '2000') data <- distritos$PRI00
    # if (partido() == 'PAN' && year() == '2000') data <- distritos$PAN00
    # if (partido() == 'PRD' && year() == '2000') data <- distritos$PRD00
    # if (partido() == 'PRI' && year() == '2006') data <- distritos$PRI06
    # if (partido() == 'PAN' && year() == '2006') data <- distritos$PAN06
    # if (partido() == 'PRD' && year() == '2006') data <- distritos$PRD06
    # if (partido() == 'PRI' && year() == '2012') data <- distritos$PRI12
    # if (partido() == 'PAN' && year() == '2012') data <- distritos$PAN12
    # if (partido() == 'PRD' && year() == '2012') data <- distritos$PRD12
    
     data <- switch(paste0(partido(),year()),
                     "PRI2000" = distritos$PRI00,
                     "PRI2006" = distritos$PRI06,
                     "PRI2012" = distritos$PRI12,
                     "PAN2000" = distritos$PAN00,
                     "PAN2006" = distritos$PAN06,
                     "PAN2012" = distritos$PAN12,
                     "PRD2000" = distritos$PRD00,
                     "PRD2006" = distritos$PRD06,
                     "PRD2012" = distritos$PRD12
                     )
    
    #cat(file=stderr(), "var", partido(), year(),  "\n")

    par(mar = c(0, 0, 1.5, 0))
    shades <- auto.shading(data, cutter = sdCuts, n = 6, cols = rev(brewer.pal(6, "RdYlBu")))
    choropleth(distritos, data, shades)
    choro.legend(-95, 32, shades, under = "<", over = ">", between = "a", box.lty = "blank", x.intersp = 0.5, y.intersp = 0.75)
    title(main = paste("Porcentaje de votos para el", partido(), "por distrito \nelectoral en", year(),"(desviaciones est??ndar)"), cex.main = 0.75)
  })
  
  ############################
  # TAB 2: NB plots
  ############################
  
  output$distritoSelect <- renderUI({
    selectInput("distrito",
                "Distrito electoral:",
                choices = as.list(unique(distritos$id)),
                selected = "1")
  })
  
  # El distrito seleccionado en el menu
  distrito <- reactive({
    if (is.null(input$distrito)) {
      return(NULL)
    }
    input$distrito
  })
  
  # neighbors plot
  output$nbPlot <- renderPlot({
    if (is.null(distrito())) {
      return()
    }
    # El distrito seleccionado
    sel.distrito <- subset(distritos, id==distrito())
    # ID del distrito seleccionado
    id <- as.integer(sel.distrito$id)
    # Los distritos vecinos al seleccionado
    subset.distritos <- distritos[distritos$id %in% distritos.nbq[[id]], ]
    
    # TODO: not sure if its ok to make outputs inside other outpus...
    output$text1 <- renderText({
      if (length(subset.distritos) > 1) vecinos <- "vecinos" else vecinos <- "vecino"
      paste(distrito(), "tiene", length(subset.distritos), vecinos, length(distritos.nbq[[id]]))
    })
    
    # Juntar los pol??gonos que nos interesan
    relevantPolys <- rbind(sel.distrito, subset.distritos)
    
    #subset.nb2 <- subset(distritos.nbq, (1:length(distritos.nbq) %in% as.integer(relevantPolys$CVE_ENT)))
    # Calcular los vecinos del poligono seleccionado
    subset.nb <- poly2nb(relevantPolys, queen = T)  # TRUE: tipo reina
    # Calcular sus centroides
    coords2 <- coordinates(relevantPolys)
    
    # Graficar el pa??s y vecinos adyacentes del estado seleccionado
    op <- par(
     oma = c(0,0,0,0),# Sin margen arriba
     mar = c(0,0,0,0)) # Sin margen entre plots
    par(bg = '#a5bfdd')
    
    plot(distritos, col = '#f1f4c7')
    # Pintar los estados que interesan
    plot(relevantPolys, col = "skyblue", add= T)
    # Destacar el seleccionado
    plot(sel.distrito, col = "tomato", add= T)
    # Pintar todos los vecinos
    plot(distritos.nbq, coords = coords, pch = 19, cex = 0.1, col = "red", add = T)
    # Pintar las conexiones del estado seleccionado
    plot(subset.nb, coords = coords2, pch = 19, lwd = 2, cex = 0.1, col = "blue", add = T)
    #plot(subset.nb2, coords = coords2, pch = 19, cex = 0.1, col = "green", add = T)
    
  })
  
  ############################
  # TAB 3: Dynamic NB map
  ############################
  
  output$NBmap <- renderLeaflet({
    NBmap <- leaflet(distritos) %>%
      addProviderTiles('CartoDB.Positron') %>%
      addPolygons(layerId = ~id, fillColor = 'yellow', #popup = popup,
                  color = 'blue', weight = 0.5, smoothFactor = 0.1)
    NBmap
  })
  
  click_distrito <- eventReactive(input$NBmap_shape_click, {
    return(input$NBmap_shape_click$id)
  })
  
  output$clicked <- renderText({ 
    click_distrito()
  })
  
  focal_distrito <- reactive({
    req(click_distrito())
    return(distritos[distritos$id == click_distrito(), ])
  })
  
  #Define neighbors for clicked polygon
  neighbors <- reactive({
    req(click_distrito())
    # Los distritos vecinos al seleccionado
    return(distritos[distritos$id %in% distritos.nbq[[click_distrito()]], ])
  })
  
  # Add nb polys
  observe({
    req(click_distrito())
    proxy <- leafletProxy('NBmap')
    if (length(neighbors()) > 0){
      if (!is.null(neighbors())) {
        proxy %>%
          removeShape('focal') %>%
          clearGroup('neighbors') %>%
          addPolygons(data = neighbors(), fill = FALSE, color = 'skyblue',
                      group = 'neighbors', opacity = 1) %>%
          addPolygons(data = focal_distrito(), color = 'tomato', 
                      opacity = 1, layerId = 'focal', fillColor = 'transparent')
      } else {
        proxy %>%
          removeShape('focal') %>%
          clearGroup('neighbors') %>%
          addPolygons(data = focal_distrito(), color = 'tomato', 
                      opacity = 1, layerId = 'focal', fillColor = 'transparent')
      }
    }  else {
      proxy %>%
        removeShape('focal') %>%
        clearGroup('neighbors')
    }
  })
  
  ############################
  # TAB 4: Dynamic LISA
  ############################
  
  partidoLISA <- reactive({ 
    as.character(input$partidoLISA) 
  })
  
  yearLISA <- reactive({ 
    as.character(input$yearLISA) 
  })

  selected <- reactive({

    # if (partidoLISA() == 'PRI' && yearLISA() == '2000') var <- distritos$PRI00
    # if (partidoLISA() == 'PAN' && yearLISA() == '2000') var <- distritos$PAN00
    # if (partidoLISA() == 'PRD' && yearLISA() == '2000') var <- distritos$PRD00
    # if (partidoLISA() == 'PRI' && yearLISA() == '2006') var <- distritos$PRI06
    # if (partidoLISA() == 'PAN' && yearLISA() == '2006') var <- distritos$PAN06
    # if (partidoLISA() == 'PRD' && yearLISA() == '2006') var <- distritos$PRD06
    # if (partidoLISA() == 'PRI' && yearLISA() == '2012') var <- distritos$PRI12
    # if (partidoLISA() == 'PAN' && yearLISA() == '2012') var <- distritos$PAN12
    # if (partidoLISA() == 'PRD' && yearLISA() == '2012') var <- distritos$PRD12
    
    var <- switch(paste0(partidoLISA(),yearLISA()),
                   "PRI2000" = distritos$PRI00,
                   "PRI2006" = distritos$PRI06,
                   "PRI2012" = distritos$PRI12,
                   "PAN2000" = distritos$PAN00,
                   "PAN2006" = distritos$PAN06,
                   "PAN2012" = distritos$PAN12,
                   "PRD2000" = distritos$PRD00,
                   "PRD2006" = distritos$PRD06,
                   "PRD2012" = distritos$PRD12
    )
    
    # Distance-based spatial weights
    knn <- reactive({
      if (input$radio == 3) {
        k <- knearneigh(coordinates(distritos), k = input$knn_slider, longlat = TRUE)
        return(k$nn)
      } else {
        return(NULL)
      }
    })
    
    distance <- reactive({
      if (input$radio == 4) {
        return(dnearneigh(coordinates(distritos), 0, input$dist_slider, longlat = TRUE))
      } else {
        return(NULL)
      }
    })
    
    # LISA Map
    distrito_weights <- reactive({
      if (input$radio == 1) {
        #return(nb2listw(include.self(poly2nb(edos))))
        return(nb2listw(poly2nb(distritos)))
      } else if (input$radio == 2) {
        #return(nb2listw(include.self(poly2nb(edos, queen = FALSE))))
        return(nb2listw(poly2nb(distritos, queen = FALSE)))
      } else if (input$radio == 3) {
        k <- knearneigh(coordinates(distritos), k = input$knn_slider, longlat = TRUE)
        #return(nb2listw(include.self(knn2nb(k))))
        return(nb2listw(knn2nb(k)))
      } else if (input$radio == 4) {
        d <- dnearneigh(coordinates(distritos), 0, input$dist_slider, longlat = TRUE)
        #return(nb2listw(include.self(d)))
        return(nb2listw(d))
      }
    })
    
    distritos$var <- var
    distritos$var_lag <- lag.listw(distrito_weights(), var)
    
    varStd <- (var - mean(var))/sd(var)
    lagStd <- lag.listw(distrito_weights(), scale(var))
    
    mean <- mean(var)
    lag_mean <- mean(distritos$var_lag) 
    
    global_moran <- moran.test(var, distrito_weights())
    statistic <- (global_moran$estimate)
    statistic <- round(statistic, 2)
    lisa <- localmoran(var, distrito_weights())
    
    distritos$cuadrante <- c(rep(0,length(var)))
    significance <- 0.05
    vec <- ifelse(lisa[,5] < significance, 1,0)
    
    distritos$cuadrante[var >= mean & distritos$var_lag >= lag_mean]  <- 1
    distritos$cuadrante[var < mean & distritos$var_lag < lag_mean]  <- 2
    distritos$cuadrante[var < mean & distritos$var_lag >= lag_mean]  <- 3
    distritos$cuadrante[var >= mean & distritos$var_lag < lag_mean]  <- 4
    distritos$cuadrante.data <- distritos$cuadrante*vec
    
    distritos$cuadrante.col[distritos$cuadrante.data==1] <- "High-High"
    distritos$cuadrante.col[distritos$cuadrante.data==2] <- "Low-Low"
    distritos$cuadrante.col[distritos$cuadrante.data==3] <- "Low-High"
    distritos$cuadrante.col[distritos$cuadrante.data==4] <- "High-Low"
    distritos$cuadrante.col[distritos$cuadrante.data==0] <- "Non-sig"
    
    distritos$fill <- factor(distritos$cuadrante.data+1)
    distritos$var_mean <- mean
    distritos$var_lag_mean <- lag_mean
    distritos$statistic <- statistic[1]
    
    distrito.sel <- subset(distritos, select = c(id, var, var_lag, cuadrante, cuadrante.data,
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
                        #values = c("1" = "white", "2" = "red", "3" = "blue", "4" = "cyan", "5" ="pink"),
                        values = c("1" = cColors[1], "2" = cColors[2], "3" = cColors[3], "4" = cColors[4], "5" =cColors[5]),
                        labels=c("Non-sig",
                                   paste0("High-High (", sum(selected()$cuadrante.data==1), ")"),  
                                 paste0("Low-Low (", sum(selected()$cuadrante.data==2), ")"),
                                 paste0("Low-High (", sum(selected()$cuadrante.data==3), ")"),
                                 paste0("High-Low (", sum(selected()$cuadrante.data==4), ")"))) +
      geom_vline(xintercept = unique(selected()$var_mean), colour = "grey", linetype = "longdash") +
      geom_hline(yintercept = unique(selected()$var_lag_mean), colour = "grey", linetype = "longdash") +
      stat_smooth(method="lm", se=FALSE, colour = "black", size = 0.5) +
      xlab("\nPorcentaje de votos por distrito electoral") +
      ylab("\nRetraso espacial deporcentaje de votos por distrito electoral") +
      theme_bw() +
      ggtitle(paste0("I de Moran: ", unique(selected()$statistic),"\n")) +
      theme(plot.title = element_text(color = "darkorchid")) 
  })
  
  output$LISAmap <- renderLeaflet({
    cColors <- c(rgb(0.74, 0.74, 0.74), rgb(1, 0, 0),
                 rgb(0, 0, 1), rgb(0.58, 0.58, 1), rgb(1, 0.58, 0.58))
    factpal <- colorFactor(cColors, 
                           domain = c("0", "1", "2", "3", "4"))
    #factpal <- colorFactor(c("#f0f0f0", "red", "blue", "cyan", "pink"), 
    #                       domain = c("0", "1", "2", "3", "4"))
    # cat(file=stderr(), "var", selected()$var, "\n")
    
    popup <- paste("% de votos para el", partidoLISA(),"en", input$yearLISA, ":", round(selected()$var,2))
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
      select(Distrito = id, '% Votos' = var, Tipo = cuadrante.col)
  },
  rownames = FALSE, options = list(pageLength = 5, dom = 'tip', autoWidth = FALSE))
  # TODO: Hide tables's div when nothing is selected... lots of white space is left in there
  
  observe({
    req(brushed())
    popup <- paste("% de votos para el", partidoLISA(),"en", input$yearLISA, ":", round(selected()$var,2))
    leafletProxy('LISAmap') %>%
    clearGroup(group = 'brushed') %>%
    addPolygons(data = brushed(), fill = "#9cf", color = '#036', weight = 1.5,
                opacity = 0.5, group = 'brushed', label = popup)
  })
  
})
