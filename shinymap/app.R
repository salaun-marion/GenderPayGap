#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(rworldmap)
library(leaflet)


#Load datasets

data <- read_csv("pay_gap_Europe.csv", show_col_types = FALSE)
colnames(data)[1]  <- "region"
colnames(data)[2]  <- "year"
worldmap <- map_data("world")
mapdata <- left_join(worldmap, data, by="region", relationship = "many-to-many")
mapdata <- mapdata %>% filter(!is.na(mapdata$GDP))
colname = c("GDP","Industry","Business","Mining", 
            "Manufacturing","Electricity_supply","Water_supply","Construction",
            "Retail trade","Transportation","Accommodation","Information",
            "Financial","Real estate","Professional_scientific",
            "Administrative","Public_administration","Education",
            "Human_health","Arts","Other")

# listEurope <- read_csv("european.csv", show_col_types = FALSE)
# colnames(listEurope)[1] <- "region"
# listEurope1 <- semi_join(worldmap, listEurope, by="region")
# 
# mapdata <- rbind.fill(listEurope1, mapdata)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Gender Pay Gap"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearCursor", "Years :",
                  min = 2010, max = 2021,
                  value = 2010, step = 2, sep = "",
                  animate = TRUE),
      selectInput("jobField", "Choose the Job field :",
                  choices=colname),
      tableOutput("view"),
    ),
    
    mainPanel(
      plotOutput(outputId = "distPlot"),
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
      output$distPlot <- renderPlot({
        
          year_map <- subset(mapdata, year==input$yearCursor)
          data <- select(year_map,long,lat,group,order,region,subregion,year,input$jobField)
          map <- ggplot(data, aes(x = long, y = lat, group = group)) +
            geom_polygon(aes(fill = !!sym(input$jobField)), color = "gray", linetype=1) +
            scale_fill_viridis_c(alpha = 0.75)+
            theme(
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              rect = element_blank()
            )
          map
       
          },width=800,height=950)
      
      output$view <- renderTable({
        year_map1 <- subset(mapdata, year==input$yearCursor)
        year_map1 = subset(year_map1, select = -c(long,lat,group,order,subregion))
        year_map1 <- year_map1 %>% distinct(.keep_all=TRUE)
        data1 <- select(year_map1,region,input$jobField)
        head(data1, n=20)
      })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
