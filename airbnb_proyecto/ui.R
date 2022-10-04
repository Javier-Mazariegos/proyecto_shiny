library(shiny)
library(readr)
library(dplyr)

df <- read_csv("Airbnb_Open_Data.csv")
df <- df[1:25]
df <- na.omit(df)


# Define UI for application that draws a histogram
shinyUI(
  
  fluidPage(
  
  titlePanel("Tabla airbnb"),
  sidebarLayout(
    sidebarPanel(
      selectInput("inReview","Select review rate",sort(unique(df$`review rate number`)))
    ),
    mainPanel(
      DT::dataTableOutput("tabla")
    )
    
  )
  
)

)
