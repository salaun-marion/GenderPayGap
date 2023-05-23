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
library(hexbin)
library(bslib)

#Load datasets

data <- read_csv("pay_gap_Europe.csv", show_col_types = FALSE)
colnames(data) <- c("region","year","GDP","Urban population","Industry","Business","Mining", 
                  "Manufacturing","Electricity supply","Water supply","Construction",
                  "Retail","Transportation","Accommodation","Information",
                  "Financial","Real estate","Science",
                  "Administrative","Public administration","Education",
                  "Health","Arts","Other")

worldmap <- map_data("world")
#glimpse(data)

mapdata <- left_join(data, worldmap, by="region", relationship = "many-to-many")

selectedcolumn = c("GDP","Industry","Business","Mining", 
                "Manufacturing","Electricity supply","Water supply","Construction",
                "Retail","Transportation","Accommodation","Information",
                "Financial","Real estate","Science",
                "Administrative","Public administration","Education",
                "Health","Arts","Other")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
  # theme 
  theme = bs_theme(version = 4, bootswatch = "minty"),
  
  # page_navbar(
  #   sidebar = sidebar("Sidebar"),
  #   nav("Page 1", map),
  #   nav("Page 2", "Page 2 content")
  # ),
  # 
  
  titlePanel("Gender Pay Gap"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearCursor", "Years :",
                  min = 2010, max = 2021,
                  value = 2010, step = 2, sep = "",
                  animate = TRUE),
      selectInput("jobField", "Choose the Job field :",
                  choices=selectedcolumn),
      tableOutput("view"),
    ),
    mainPanel(
      tabsetPanel(type ="tab",
                  tabPanel("Map",plotOutput("distPlot")),
                  tabPanel("Data",tableOutput("distData"))
      )
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
            scale_fill_viridis_c(alpha = 0.75, option="A")+
            theme(
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              rect = element_blank()
            )
          map
       
          },width=600,height=750)
      
      output$view <- renderTable({
        year_map1 <- subset(mapdata, year==input$yearCursor,select = -c(long,lat,group,order,subregion))
        year_map1 <- year_map1 %>% distinct(.keep_all=TRUE)
        data1 <- select(year_map1,region,input$jobField)
        data1 <- data1[order(data1$region),]
        
        head(data1, n=10)
      })
      
      output$distData <- renderTable(
        data
      )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
