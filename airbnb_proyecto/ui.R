library(shiny)
library(shinyjs)
library(shinyWidgets)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(gapminder)

df <- read_csv("Airbnb_Open_Data.csv")
df <- df[1:25]
df <- na.omit(df)
df$price <- str_sub(df$price,2,-1)
df$price <- as.numeric(gsub(",", "", df$price))


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
                         ),
                         column(3,
                                selectInput("inHostName","Select host name: ",c("Todos",unique(df$`host name`))),
                                selectInput("inNeighbourHood","Select neighbourhood group: ",c("Todos",unique(df$`neighbourhood group`)))
                         ),
                         column(4,
                                checkboxGroupInput('chkbox_group_input',
                                                   'Select room type:',
                                                   choices = unique(df$`room type`),
                                                   selected = NULL,inline = TRUE),
                                br(),
                                sliderInput('inPrice','Select price:',
                                            value = c(min(df$price),max(df$price)),
                                            min = min(df$price), 
                                            max=max(df$price),
                                            step = 1,
                                            pre = '$',
                                            sep = ',' )
                                
                         ),
                         column(2,
                                numericInput("inAvil","Availability 365:",
                                             value = 0, step = 1 )
                                
                         ),
                       ),
                      fluidRow(
                        div(DT::dataTableOutput("tabla")),
                        style = "font-size: 98%; width: 100%"
                      )
               )
      ),
      tabPanel("Graficas", icon = icon("chart-area"), value = "trend", 
        
        sidebarPanel(width=4 ,
            useShinyjs(),
            selectInput("inGrafica","Seleccione el tipo de grafica: ",c("Elegir...","Cantidad ubicaciones vs precio promedio","Calidad Precio","Casas con mejores reviews","Verificados vs No verificados","Cantidad de casas por vecindario")),
            numericInput("inGraficaNumerico","Availability 365:", value = 3, step = 1 )
          
        ),
        mainPanel(width = 8,
                  plotlyOutput("plotGrafica") 
        )
               
      ),
      tabPanel("Mapas", tableOutput("table")),

      tabPanel("Casa URL", icon = icon("home"), value = "home",
               useShinyjs(),
               mainPanel(id="Casa URL",
                 width = 12, style="margin-left:0.5%; margin-right:0.5%",
                 fluidRow(
                   column(4,
                          div(style = "height:70vh; display: flex; align-items: center; justify-content: center;",
                              htmlOutput("picture"))
                   ),
                   column(4,
                          div(style = "height:80vh; padding-top: 45px;",
                              p("id", style = "font-weight: bold; color: black;"),
                              textOutput("id_text"),
                              p("Nombre de la casa", style = "font-weight: bold; color: black;"),
                              textOutput("nombreC_text"),
                              p("Verificacion", style = "font-weight: bold; color: black;"),
                              textOutput("Verificacion_text"),
                              p("Nombre del anfitrion", style = "font-weight: bold; color: black;"),
                              textOutput("nombreA_text"),
                              p("Grupo de vecino", style = "font-weight: bold; color: black;"),
                              textOutput("grupoV_text"),
                              p("Vecindario", style = "font-weight: bold; color: black;"),
                              textOutput("vecindario_text"),
                              p("Pais", style = "font-weight: bold; color: black;"),
                              textOutput("pais_text"),
                              )
                   ),
                   column(4,
                          div(style = "height:80vh; padding-top: 45px;",
                              p("Reserva instantanea", style = "font-weight: bold; color: black;"),
                              textOutput("reserva_text"),
                              p("Politica de cancelacion", style = "font-weight: bold; color: black;"),
                              textOutput("politica_text"),
                              p("Tipo de habitacion", style = "font-weight: bold; color: black;"),
                              textOutput("tipoA_text"),
                              p("Año de construccion", style = "font-weight: bold; color: black;"),
                              textOutput("Aconstruccion_text"),
                              p("Precio", style = "font-weight: bold; color: black;"),
                              textOutput("precio_text"),
                              p("Tarifa de servicio", style = "font-weight: bold; color: black;"),
                              textOutput("tarifa_text"),
                              p("Minimo de noches", style = "font-weight: bold; color: black;"),
                              textOutput("min_text"),
                              p("Review", style = "font-weight: bold; color: black;"),
                              textOutput("review_text"))
                   )
                ),
                 fluidRow(
                   column(12,
                          div(style = "height:10vh;",
                              p("Reglas", style = "font-weight: bold; color: black;"),
                              textOutput("reglas_text"))
                   )
               )
            
          )
        ) 
      )
    )
  )
)
