library(shiny)
library(dplyr)
library(ggplot2)

df <- read_csv("Airbnb_Open_Data.csv")
df <- df[1:25]
df <- na.omit(df)
df$price <- str_sub(df$price,2,-1)
df$price <- as.numeric(gsub(",", "", df$price))
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
            tabla <- tabla %>% filter(`host name` %in% input$inNeighbourHood) 
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

})
