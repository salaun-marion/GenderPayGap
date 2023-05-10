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

pay_gap_Europe <- read_csv("pay_gap_Europe.csv", show_col_types = FALSE)
data <- data.frame(countries = pay_gap_Europe$Country, GDP = pay_gap_Europe$GDP,
                   pay_gap_Europe$Year)
colnames(data)[1]  <- "region"
colnames(data)[3]  <- "year"
mapdata <- map_data("world")
mapdata <- left_join(mapdata, data, by="region", relationship = "many-to-many")
mapdata1 <- mapdata %>% filter(!is.na(mapdata$GDP))


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Gender Pay Gap"),
  
  # Sidebar with a slider input
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearCursor", "Years :",
                  min = 2010, max = 2022,
                  value = 2010, step = 1, sep = "",
                  animate = TRUE),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(outputId = "distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    data <- reactive({
      year_map <- subset(mapdata1, year==input$yearCursor)
    })
    
    ggplot(data = data(), aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = GDP), color = "black") +
      scale_fill_gradient(
        name = "Gender pay gap with GDP",
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