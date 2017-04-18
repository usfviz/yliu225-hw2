library(shiny)
library(ggvis)
library(reshape2)
library(tidyr)

ui <- fluidPage(
  headerPanel('Life Expectancy and Fertility Rate'),
  sidebarPanel(
    sliderInput("year", "Year", 1960,2016,1),
    
    checkboxGroupInput("rg","Regions",choices = regions)
    
  ),
  mainPanel(
    uiOutput("ggvis_ui"),
    ggvisOutput("ggvis")
  )
)