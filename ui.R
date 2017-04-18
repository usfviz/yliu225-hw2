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
FUN_2 <- function(num) {num/900000}
df$population <- lapply(df$population, FUN_2)



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