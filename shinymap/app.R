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
library(shinydashboard)
library(reshape2)

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

allcolumnNames = c(colnames(data))

selectedcolumn = c("GDP","Industry","Business","Mining", 
                "Manufacturing","Electricity supply","Water supply","Construction",
                "Retail","Transportation","Accommodation","Information",
                "Financial","Real estate","Science",
                "Administrative","Public administration","Education",
                "Health","Arts","Other")

selectedcountries = c(unique(data$region))

# Header ----
header <- dashboardHeader(title="Gender Pay Gap")

# SideBar ----

sidebar <- dashboardSidebar(
  conditionalPanel(condition="input.tabselected==1",
                   sliderInput("yearCursor", "Years :",
                               min = 2010, max = 2021,
                               value = 2010, step = 2, sep = "",
                               animate = TRUE),
                   selectInput("jobField", "Choose the Job field :",
                               choices=selectedcolumn),
  ),
  
  conditionalPanel(condition="input.tabselected==2",
                   checkboxGroupInput("country","Select Country:",
                                      choices = selectedcountries,
                                      selected = c("France"),
                   ),
  ),
  conditionalPanel(condition="input.tabselected==3",
                   selectInput("variables","Select variables:",
                                      choices = allcolumnNames,
                                      selected = c("region"),
                                      multiple=TRUE,
                   ),
  )
)

# Body ----
body <- dashboardBody(
  mainPanel(
    tabsetPanel(
      tabPanel("Map", value=1,
                 fluidRow(
                   box(title = "Map of Europe", height = 650,width = 7, plotOutput("distPlot")),
                   box(width = 5, height = 650, tableOutput("viewCountries")),
                 )
               ),
      tabPanel("Plot", value=2,
               fluidRow(
                   box(width = 12,plotOutput("linePlot")),
                )
              ),
      tabPanel("Data", value=3,
               tableOutput("distData")),
      id = "tabselected"
    )
  ),
  tags$head(tags$style(HTML('
                                .main-sidebar{
                                  width: 200px;
                                }
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff;
                                }
                                
                                ')))
)
ui <- dashboardPage(header, sidebar, body, skin='purple')


# Define server logic required to draw a histogram
server <- function(input, output) {
  
      #create the map
      output$distPlot <- renderPlot({
          year_map <- subset(mapdata, year==input$yearCursor)
          data <- year_map %>%
            dplyr::select(long, lat, group, order, region, subregion, year, !!input$jobField)
          
          
          map <- ggplot(data, aes(x = long, y = lat, group = group)) +
            geom_polygon(aes(fill = !!sym(input$jobField)), color = "gray", linetype=1) +
            scale_fill_viridis_c(alpha = 0.75, option="A")+
            theme(
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              rect = element_blank(),
            )
          map
       
          })
      
      #create the table closed to the map
      output$viewCountries <- renderTable({
        year_map1 <- subset(mapdata, year==input$yearCursor,select = -c(long,lat,group,order,subregion))
        year_map1 <- year_map1 %>% distinct(.keep_all=TRUE)
        data1 <- year_map1 %>%
          dplyr::select(region, !!input$jobField)
        data1 <- data1[order(data1$region),]
        head(data1, n = 18)
      })
      
      #Line plot
      output$linePlot <- renderPlot({
        
        countryname <- req(input$country)
        
        plotLineData <- data %>%
          pivot_longer(cols = -c("region","year","GDP","Urban population"), names_to = 'Domain', values_to = 'GPG')
        
        plotLineData2 <- plotLineData %>%
          mutate(JobSectors = case_when(Domain %in% c('Retail') ~ 'Trade and commerce',
                                        Domain %in% c('Manufacturing') ~ 'Manufacturing and production',
                                        Domain %in% c('Electricty supply', 'Water supply','Mining', 'Construction') ~ 'Primary Industry and Infrastructure',
                                        Domain %in% c("Business","Transportation","Accommodation","Information",
                                                      "Financial","Real estate","Science",
                                                      "Administrative") ~ 'Service and information',
                                        is.na(Domain) ~ 'Public sector and social services',
                                        TRUE ~ 'Others'
          ))%>%
          filter(region %in% countryname) %>%
          group_by(region,JobSectors, year) %>%
          summarize(GPG=mean(GPG,na.rm = TRUE))
        plotLineData2 %>%
          ggplot() +
          geom_point(aes(x = year, y = GPG, color = (JobSectors), group= JobSectors)) +
          geom_line(aes(x = year, y = GPG, color = (JobSectors), group= JobSectors)) +
          facet_wrap(vars(region), ncol=5) +
          theme_bw()

      },width=500,height=400)
      
      #show dataset
      output$distData <- renderTable(
        data %>%
          dplyr::select(unlist(input$variables, use.names = FALSE))
      )
      
      
    
}

# Run the application 
shinyApp(ui = ui, server = server)
