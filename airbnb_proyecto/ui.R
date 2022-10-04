library(shiny)
library(readr)
library(dplyr)

df <- read_csv("Airbnb_Open_Data.csv")
df <- df[1:25]
df <- na.omit(df)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  mainPanel(
    tabsetPanel(
      tabPanel("Tabla", icon = icon("table"), value = "table", 
               mainPanel(
                 width = 12, style="margin-left:0.5%; margin-right:0.5%",
                       fluidRow(
                         p("Filtros", 
                           style = "font-weight: bold; color: black;"),
                         p("Seleccione al menos un filtro para mostrar la tabla. "),
                         br()
                       ),
                       fluidRow(
                         selectInput("inReview","Select review rate",sort(unique(df$`review rate number`)))
                       ),
                      fluidRow(
                        div(DT::dataTableOutput("tabla")),
                        style = "font-size: 98%; width: 98%"
                      )
               )
      ),
      tabPanel("Graficas", verbatimTextOutput("summary")),
      tabPanel("Mapas", tableOutput("table"))
    )
  )
  
  
  
)

)
