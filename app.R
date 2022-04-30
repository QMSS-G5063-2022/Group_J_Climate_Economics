library(shiny)
setwd("~/Documents/GitHub/Group_J_Climate_Economics/")

df =  read.csv('final_to_use.csv')
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)

country2015 <- df%>%
  filter(Year==2015)

col_choice_list = c('GDP','tmean','Population','Frequency')


library(shiny)

ui <- fluidPage(
  titlePanel("Group J - Climate Change and Economics"),
  selectInput("Ind","Independent Variable",choices = col_choice_list),
  plotOutput('Hist'),
  
  )
server <- function(input, output, session) {
  
  data1 <- reactive({
    input$Ind
  })
  
  output$Hist <- renderPlot({
    req(data1())
    hist(df[[data1()]], col = "#75AADB", border = "white",
         xlab = " ",
         main = "Histogram ")
  }) 
  
}

shinyApp(ui, server)