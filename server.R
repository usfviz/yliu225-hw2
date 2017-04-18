library(shiny)
library(ggvis)
library(reshape2)
library(tidyr)


life_exp <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", skip=3, header=TRUE)
fertility <- read.csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", skip=3, header=TRUE)
population = read.csv("population.csv")
region <- read.csv("Metadata_Country_API_SP.POP.1564.TO.ZS_DS2_en_csv_v2.csv",header=TRUE)


life_exp_long <- gather(life_exp, year, life_expectancy, X1960:X2016, factor_key=TRUE)
life_exp_long <- life_exp_long[,c(1,2,6,7)]
fertility_long <- gather(fertility, year, fertility_rate, X1960:X2016, factor_key=TRUE)
fertility_long <- fertility_long[,c(1,2,6,7)]
population_long <- gather(population, year, population, X1960:X2016, factor_key=TRUE)
population_long <- population_long[,c(1,2,5,6)]
df <- merge(life_exp_long, fertility_long, by=c("Country.Code", "year"))
df<- merge(df, population_long, by=c("Country.Code", "year"))




FUN <- function(s) {as.numeric(substr(s, 2, 5))}
df$year <- lapply(df$year, FUN)


colnames(region)[1] <- "Country.Code"
region <- region[,c(1,2)]
df<- merge(df, region, by=c("Country.Code"))
df <- df[,c(1,2,4,6,7,8,9)]
regions <- levels(as.factor(df$Region))
#FUN_2 <- function(num) {num/900000}
#df$population <- lapply(df$population, FUN)





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
    ggvis(~fertility_rate, ~life_expectancy, fill=~regions) %>%
    layer_points(fill = ~Region,size := ~population/1000000 + 75, stroke := "black") %>%
    layer_points(data = rg,fill = "black",size := ~population/1000000 + 75, stroke := "white") %>%
    add_tooltip(all_values, "hover") %>%
    bind_shiny("ggvis", "ggvis_ui")
  
}