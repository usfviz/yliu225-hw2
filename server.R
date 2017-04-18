library(shiny)
library(ggvis)
library(reshape2)
library(tidyr)


server <- function(input, output) {
  
  df_year <- reactive({tmp = subset(df, year == input$year)})
  
  rg <- reactive({tmp = df[df$Region == input$rg,]
  tmp = tmp[tmp$year == input$year,]
  tmp})
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    tmp = df_year()
    dot <- tmp[tmp$fertility_rate == x$fertility_rate,]
    dot$Country.Name
  }
  
  df_year %>% 
    ggvis(~fertility_rate, ~life_expectancy) %>%
    add_tooltip(all_values, "hover") %>%
    group_by(Region) %>% 
    layer_points(fill = ~Region,size := ~population/1000000 + 75) %>%
    layer_points(data = rg,fill = "black",size := ~population/1000000 + 75) %>%
    bind_shiny("ggvis", "ggvis_ui")
  
}