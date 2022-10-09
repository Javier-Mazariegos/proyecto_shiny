library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(gapminder)
library(shinyjs)
library(readr)
library(stringr)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

df <- read_csv("Airbnb_Open_Data.csv")
df <- df[1:25]
df <- na.omit(df)
df$price <- str_sub(df$price,2,-1)
df$price <- as.numeric(gsub(",", "", df$price))
i <- reactiveVal()
numeric_val <- reactiveVal()
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
    observe({
      query <- parseQueryString(session$clientData$url_search)
      id_query <- query[["id"]]
      toggle(id = "Casa URL", condition = !is.null(id_query))
      if(!is.null(id_query)){
       dataF <- df %>% filter(id == id_query)
       output$id_text <- renderText({ dataF$id })
       output$nombreC_text <- renderText({ dataF$NAME })
       output$Verificacion_text <- renderText({ dataF$host_identity_verified })
       output$Verificacion_text <- renderText({ dataF$host_identity_verified })
       output$nombreA_text <- renderText({ dataF$`host name` })
       output$grupoV_text <- renderText({ dataF$`neighbourhood group` })
       output$vecindario_text <- renderText({ dataF$neighbourhood })
       output$pais_text <- renderText({ dataF$country })
       
       output$reserva_text <- renderText({ dataF$instant_bookable })
       output$politica_text <- renderText({ dataF$cancellation_policy })
       output$tipoA_text <- renderText({ dataF$`room type` })
       output$Aconstruccion_text <- renderText({ dataF$`Construction year` })
       output$precio_text <- renderText({ dataF$price })
       output$tarifa_text <- renderText({ dataF$`service fee` })
       output$min_text <- renderText({ dataF$`minimum nights` })
       output$review_text <- renderText({ dataF$`review rate number` })
       
       output$reglas_text <- renderText({ dataF$house_rules })
       
       src <- "https://assets-news.housing.com/news/wp-content/uploads/2022/04/07013406/ELEVATED-HOUSE-DESIGN-FEATURE-compressed.jpg"
       output$picture <- renderText({c('<img src="',src,'" width="400">')})
      }
    })
  
    output$tabla <- DT::renderDataTable({
      tabla <- df  
        if(input$inReview != ""){
          if(input$inReview == "Todos"){
            tabla <- tabla 
          }
          else{
            tabla <- tabla %>% filter(`review rate number` %in% input$inReview) 
          }
        }
        if(!is.null(input$inVerificacion)){
          if(input$inVerificacion == "ambos"){
            tabla <- tabla 
          }
          else{
          tabla <- tabla %>% filter(host_identity_verified %in% input$inVerificacion)
          }
        }
        if(input$inHostName != ""){
          if(input$inHostName == "Todos"){
            tabla <- tabla 
          }
          else{
            tabla <- tabla %>% filter(`host name` %in% input$inHostName) 
          }
        }
        if(input$inNeighbourHood != ""){
          if(input$inNeighbourHood == "Todos"){
            tabla <- tabla 
          }
          else{
            tabla <- tabla %>% filter(`neighbourhood group` %in% input$inNeighbourHood) 
          }
        }
        if(!is.null(input$chkbox_group_input)){
            tabla <- tabla %>% filter(`room type` %in% input$chkbox_group_input)
        }
        if(input$inAvil != 0){
            tabla <- tabla %>% filter(`availability 365` %in% input$inAvil) 
        }
        
        ifelse(input$inPrice != 0,tabla <- tabla %>% filter(between(price,input$inPrice[1],input$inPrice[2])),tabla)
        
        
         
        tabla <- tabla %>% select(NAME,`review rate number`,host_identity_verified,`host name`,`neighbourhood group`,`room type`,price,`availability 365`) %>%
        DT::datatable(options = list(searching=FALSE,bLengthChange =FALSE))
    })
    
    observeEvent(input$inGrafica, { 
      if(input$inGrafica == "Elegir..."){
        i(NULL)
      }
      if(input$inGrafica == "Cantidad ubicaciones vs precio promedio"){
        p <- df %>% select(`minimum nights`,price) %>% group_by(`minimum nights`) %>% summarise(precio_promedio = mean(price)) %>% filter(`minimum nights`>0)
        p$precio_promedio <-as.double(p$precio_promedio)
        p <- p %>%
          ggplot( aes(`minimum nights`, precio_promedio, color=precio_promedio)) +
          geom_point() +
          theme_bw()
        ggplotly(p)
        i(p)
      }
      if(input$inGrafica == "Calidad Precio"){
        p <- df %>% filter(df$`review rate number`==5) %>% arrange(price) %>% head(input$inGraficaNumerico)
        p <- p %>% plot_ly(
          x = p$NAME,
          y = p$price,
          name = "SF Zoo",
          type = "bar"
        )
        i(p)
      }
      
      if(input$inGrafica == "Casas con mejores reviews"){
        p <- df %>% filter(df$`review rate number`==5) %>% arrange(desc(`number of reviews`)) %>% head(input$inGraficaNumerico)
        p <- p %>% plot_ly(
          x = p$NAME,
          y = p$`number of reviews`,
          name = "SF Zoo",
          type = "bar"
        )
        i(p)
      }
      if(input$inGrafica == "Verificados vs No verificados"){
        p <- df %>% group_by(host_identity_verified) %>% summarise(porcentaje = (n()/nrow(df))*100)
        p <- plot_ly(p, labels = ~host_identity_verified, values = ~porcentaje, type = 'pie')
        p <- p %>% layout(title = 'Porcentaje de casas verificadas vs no verificadas',
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        i(p)
      }
      if(input$inGrafica == "Cantidad de casas por vecindario"){
        p <- df %>% group_by(`neighbourhood group`) %>% count(name = "No_casas")
        p <- plot_ly(
          type = 'scatterpolar',
          r = p$No_casas,
          theta = p$`neighbourhood group`,
          fill = 'toself'
        ) 
        p <- p %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T,
                range = c(0,max(t$No_casas)+1000)
              )
            ),
            showlegend = F
          )
        i(p)
      }
      
    })
    
    observeEvent(input$inGraficaNumerico, { 
      if(input$inGrafica == "Calidad Precio"){
        p <- df %>% filter(df$`review rate number`==5) %>% arrange(price) %>% head(input$inGraficaNumerico)
        p <- p %>% plot_ly(
          x = p$NAME,
          y = p$price,
          name = "SF Zoo",
          type = "bar"
        )
        i(p)
      }
      
      if(input$inGrafica == "Casas con mejores reviews"){
        p <- df %>% filter(df$`review rate number`==5) %>% arrange(desc(`number of reviews`)) %>% head(input$inGraficaNumerico)
        p <- p %>% plot_ly(
          x = p$NAME,
          y = p$`number of reviews`,
          name = "SF Zoo",
          type = "bar"
        )
        i(p)
        
      }
      
      
      
    })
    
    output$plotGrafica <- renderPlotly({
        i()
    })
    
    
    observe({
      toggle(id = "inGraficaNumerico", condition = (input$inGrafica  == "Calidad Precio") | (input$inGrafica  == "Casas con mejores reviews"))
    })
    
    ## Mapa ###########################################
    
    # Creando mapa
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = -73.9734393, lat = 40.6982127, zoom = 14)
    })
    
    # limites
    pricesInBounds <- reactive({
      if (is.null(input$map_bounds))
        return(df[FALSE,])
      bounds <- input$map_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      subset(df,
             lat >= latRng[1] & lat <= latRng[2] &
               long>= lngRng[1] & long <= lngRng[2])
    })
    
    
    # seleccion de data para colores
    observe({
      colorBy <- input$color
      
      colorData <- df[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
      
      radius <- 30
      
      leafletProxy("map", data = df) %>%
        clearShapes() %>%
        addCircles(~long, ~lat, radius=radius, layerId=~id,
                   stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
    })
    
    # Ventana emergente
    showIdcodePopup <- function(id, lat, lng) {
      selectedId <- df[df$id == id,]
      content <- as.character(tagList(
        tags$h4(as.character(selectedId$NAME)),
        tags$strong(HTML(sprintf("%s, %s  ID: %s",
                                 selectedId$`neighbourhood group`, selectedId$neighbourhood, selectedId$id
        ))), tags$br(),
        tags$strong(sprintf("Price: %s", dollar(selectedId$price))), tags$br(),
        sprintf("Host name: %s", as.character(selectedId$`host name`)), tags$br(),
        sprintf("Minimum nights: %s", as.integer(selectedId$`minimum nights`)), tags$br(),
        sprintf("Number of reviews: %s", as.integer(selectedId$`number of reviews`)), tags$br(),
        sprintf("Review rate number: %s", as.integer(selectedId$`review rate number`)), tags$br(),
        sprintf("Availability 365: %s", as.integer(selectedId$`availability 365`)), tags$br(),
        sprintf("House Rules: %s", as.character(selectedId$house_rules)), tags$br()
      ))
      leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id)
    }
    
    # Mostrar ventana emergente
    observe({
      leafletProxy("map") %>% clearPopups()
      event <- input$map_shape_click
      if (is.null(event))
        return()
      
      isolate({
        showIdcodePopup(event$id, event$lat, event$lng)
      })
    })

})
