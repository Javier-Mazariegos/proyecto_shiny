FROM rocker/shiny:latest

RUN R -e "install.packages(c('DT','sys','shinydashboard' ,'dplyr','ggplot2','gapminder', 'shinyjs','readr','stringr','leaflet','RColorBrewer','scales','lattice','shiny', 'shinyWidgets', 'plotly'))"

COPY / /app

EXPOSE 8000

CMD ["R","-e","shiny::runApp('/app',host= '0.0.0.0',port=8000)"]