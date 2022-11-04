FROM rocker/shiny:latest

RUN R -E "install.packages(c('dplyr','rpart'))"

EXPOSE 8000

CMD ["R","-e","shiny::runApp('/app',host= '0.0.0.0',port=8000)"]