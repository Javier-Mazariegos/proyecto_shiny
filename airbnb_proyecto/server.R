library(shiny)
library(dplyr)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$tabla <- DT::renderDataTable({
    subset(df, df$`review rate number` == input$inReview) %>% DT::datatable()
    })

})
