#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(tidyverse)
library(maps)
library(rworldmap)
library(dplyr)

#Load dataset

data <- read_csv("pay_gap_Europe.csv", show_col_types = FALSE)
colnames(data)[1]  <- "region"
colnames(data)[2]  <- "year"
mapdata <- map_data("world")
mapdata <- left_join(mapdata, data, by="region", relationship = "many-to-many")
mapdata <- mapdata %>% filter(!is.na(mapdata$GDP))
colname = c("GDP","Industry","Business","Mining", 
            "Manufacturing","Electricity_supply","Water_supply","Construction",
            "Retail trade","Transportation","Accommodation","Information",
            "Financial","Real estate","Professional_scientific",
            "Administrative","Public_administration","Education",
            "Human_health","Arts","Other")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Gender Pay Gap"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearCursor", "Years :",
                  min = 2010, max = 2022,
                  value = 2010, step = 1, sep = "",
                  animate = TRUE),
      selectInput("jobField", "Choose the Job field :",
                  choices=colname),
    ),

    mainPanel(
      plotOutput(outputId = "distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    data <- reactive({
      year_map <- subset(mapdata, year==input$yearCursor)
      selected_map <- select(year_map,long,lat,group,order,region,subregion,year,input$jobField)
    })
    
    ggplot(data = data(), aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = !!sym(input$jobField)), color = "black") +
      scale_fill_gradient(
        low = "yellow", high = "purple", na.value = "grey"
      ) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()
      )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
