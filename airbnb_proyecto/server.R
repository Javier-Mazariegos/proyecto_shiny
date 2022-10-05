library(shiny)
library(dplyr)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

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
         
        tabla <- tabla %>% select(NAME,`review rate number`,host_identity_verified,`host name`,`neighbourhood group`,`room type`,price,`availability 365`) %>%
        DT::datatable(options = list(searching=FALSE,bLengthChange =FALSE))
    })

})
