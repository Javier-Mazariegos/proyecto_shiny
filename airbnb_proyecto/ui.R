library(shiny)
library(shinyWidgets)
library(readr)
library(dplyr)

df <- read_csv("Airbnb_Open_Data.csv")
df <- df[1:25]
df <- na.omit(df)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  mainPanel( width=100,
    tabsetPanel(
      tabPanel("Tabla", icon = icon("table"), value = "table", 
               mainPanel(
                 width = 12, style="margin-left:0.5%; margin-right:0.5%",
                       fluidRow(
                         br(),
                         p("Filtros", 
                           style = "font-weight: bold; color: black;"),
                         p("Seleccione al menos un filtro para mostrar la tabla. "),
                         br()
                       ),
                       fluidRow(
                         column(3,
                                radioGroupButtons(
                                  inputId = "inVerificacion",
                                  label = "Verificación de anfitrion: ",
                                  choices = c("ambos","verified", "unconfirmed"),
                                  selected = character(0)
                                ),
                                selectInput("inReview","Select review rate: ",c("Todos",sort(unique(df$`review rate number`))))
                         )
                       ),
                      fluidRow(
                        div(DT::dataTableOutput("tabla")),
                        style = "font-size: 98%; width: 100%"
                      )
               )
      ),
      tabPanel("Graficas", verbatimTextOutput("summary")),
      tabPanel("Mapas", tableOutput("table"))
    )
  )
  
  
  
)

)
