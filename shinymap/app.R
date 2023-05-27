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
library(RColorBrewer)
library(hexbin)
library(bslib) # for convenient window arrangement in plotting
library(gridExtra)

#Load datasets

## -- FOR THE MAP
data <- read_csv("pay_gap_Europe.csv", show_col_types = FALSE)
colnames(data) <- c("region","year","GDP","UrbanPopulation","Industry","Business","Mining", 
                  "Manufacturing","ElectricitySupply","WaterSupply","Construction",
                  "Retail","Transportation","Accommodation","Information",
                  "Financial","RealEstate","Science",
                  "Administrative","PublicAdministration","Education",
                  "Health","Arts","Other","Average")

worldmap <- map_data("world")
#glimpse(data)
mapdata <- left_join(data, worldmap, by="region", relationship = "many-to-many")

## -- SET THE DIFFERENT VARIABLES

central_europe <- c("France","Luxembourg","Netherlands","Belgium","Germany", "Austria", "Czech Republic", "Hungary", "Poland", "Slovakia", "Slovenia")
northern_europe <- c("Denmark", "Estonia", "Finland", "Iceland", "Latvia", "Lithuania", "Norway", "Sweden")
eastern_europe <- c("Bulgaria", "Croatia", "Romania")
southern_europe <- c("Cyprus", "Greece", "Italy", "Malta", "Portugal", "Spain")

allcolumnNames = c(colnames(data))
selectedcolumn = c("GDP","Industry","Business","Mining", 
                "Manufacturing","Electricity supply","Water supply","Construction",
                "Retail","Transportation","Accommodation","Information",
                "Financial","Real estate","Science",
                "Administrative","Public administration","Education",
                "Health","Arts","Other")

selectedcountries = c(unique(data$region))

## -- FOR THE MATRIX

pay_gap_Europe <- read.csv("pay_gap_Europe.csv")

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
                                      selected = c("France"),),
                   actionLink("selectall","Select All"),
                   # actionLink("reset","Reset") 
  ),
  conditionalPanel(condition="input.tabselected==3",
                   selectInput("variables","Select variables:",
                                      choices = allcolumnNames,
                                      selected = c("region"),
                                      multiple=TRUE,
                   ),
  ),
  conditionalPanel(condition="input.tabselected==4",
                   selectInput("variablesOne","Select variables:",
                               choices = allcolumnNames,
                               selected = c("Average"),
                            
                   ),
                   selectInput("variablesTwo","Select variables:",
                               choices = allcolumnNames,
                               selected = c("UrbanPopulation"),
                             
                   ),
  )
  # conditionalPanel(condition="input.tabselected==5",
  #                  selectInput("variables","Select variables:",
  #                              choices = allcolumnNames,
  #                              selected = c("region"),
  #                              multiple=TRUE,
  #                  ),
  # )
)

# Body ----
body <- dashboardBody(
  mainPanel(
    tabsetPanel(
      tabPanel("Map", value=1,
                   fluidRow(
                     box(title = "Map of Europe", height = 500,width = 6, plotOutput("distPlot")),
                     box(width = 3, height = 500, tableOutput("viewCountries1")),
                     box(width = 3, height = 500, tableOutput("viewCountries2")),
                     div(HTML("<b>Gender Pay Gap (GPG)</b> is computed as : 
                     <em>(Men's Average Earnings - Women's Average Earnings) / Men's Average Earnings</em> 
                       which means if GPG = 0.20, women earn 20% less than men in the same company."),style="text-align:center")
                   )
               ),
      tabPanel("Plot", value=2,
                  plotOutput("linePlot"),
              ),
      tabPanel("Correlation", value=3, plotOutput("corrMatrix")),
      tabPanel("Box plot", value=4,plotOutput("boxPlot")),
      # tabPanel("Data", value=5,
      #          tableOutput("distData")),
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
server <- function(input, output, session) {
  
      #create the map
      output$distPlot <- renderPlot({
          year_map <- subset(mapdata, year==input$yearCursor)
          mapdata <- year_map %>%
            dplyr::select(long, lat, group, order, region, subregion, year, !!input$jobField)
          
          
          map <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
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
      output$viewCountries1 <- renderTable({
        data1 <- subset(mapdata,year == input$yearCursor)
        data1 <- data1 %>% dplyr::select(region, !!input$jobField)
        data1<-data1 %>% distinct(.keep_all=TRUE)
        
        n=13
        firstTable <- data1[row.names(data1) %in% 1:n, ]
        head(firstTable, n )
      })
      output$viewCountries2 <- renderTable({
        data1 <- subset(mapdata,year == input$yearCursor)
        data1 <- data1 %>% dplyr::select(region, !!input$jobField)
        data1 <- data1 %>% distinct(.keep_all=TRUE)
        
        n=13
        secondtTable <- data1[row.names(data1) %in% (n+1):nrow(data1), ]
        head(secondtTable, n)
      })
      
      #Line plot
      output$linePlot <- renderPlot({
        
        countryname <- req(input$country)
        
        observe({
          if(input$selectall == 0){ 
            return(NULL) 
          }else{
            updateCheckboxGroupInput(session,"country","Select Country:",
                                     choices = selectedcountries,
                                     selected = selectedcountries)
          }
       
        })
        
        plotLineData <- data %>%
          pivot_longer(cols = -c("region","year","GDP","UrbanPopulation","Average"), names_to = 'Domain', values_to = 'GPG')
        
        plotLineData2 <- plotLineData %>%
          mutate(JobSectors = case_when(Domain %in% c('Retail') ~ 'Trade and commerce',
                                        Domain %in% c('Manufacturing') ~ 'Manufacturing and production',
                                        Domain %in% c('ElectrictySupply', 'WaterSupply','Mining', 'Construction') ~ 'Primary Industry and Infrastructure',
                                        Domain %in% c("Business","Transportation","Accommodation","Information",
                                                      "Financial","RealEstate","Science",
                                                      "Administrative") ~ 'Service and information',
                                        is.na(Domain) ~ 'Public sector and social services',
                                        TRUE ~ 'Others'
          ))%>%
          filter(region %in% countryname) %>%
          group_by(region,JobSectors,year) %>%
          summarize(GPG=mean(GPG,na.rm = TRUE))
        
        plotLineData2 %>%
          ggplot() +
          geom_point(aes(x = year, y = GPG, color = (JobSectors), group = JobSectors)) +
          geom_line(aes(x = year, y = GPG, color = (JobSectors), group = JobSectors)) +
          facet_wrap(vars(region), ncol=5) +
          theme_bw()

      },width=900,height=800)
      
      #Correlation matrix
      output$corrMatrix <- renderPlot({
        
        #create a new column with the country
        pay_gap_Europe$Country_factor <- as.factor(pay_gap_Europe$Country)
        #give to each country a qualitative value
        pay_gap_Europe$Country_numeric <- as.numeric(pay_gap_Europe$Country_factor) - 1
        pay_gap_Europe
        
        #remove from the past dataset the qualitative values
        pay_gap<-subset(pay_gap_Europe,select=-c(Country, Country_factor) )
        pay_gap
        
        #see the correlation of pay_gap
        pay_gap.corr<-cor(na.omit(pay_gap))
        pay_gap.corr
        
        #Make a graph for the correlation
        palette <- colorRampPalette(brewer.pal(8, "PuOr"))(25)
        
        heatmap(x = pay_gap.corr,
                # Colv = NA,
                # Rowv = NA,
                cexRow = 0.8,
                cexCol = 0.8,
                labCol = allcolumnNames,
                labRow = allcolumnNames,
                col= palette,
                scale="column", # scale allow the normalization
                symm = TRUE)
        legend(x = "bottomright", c("Max", "Mid","Min"), fill = c("Purple","Beige","Brown"))
        
      })
      
      #Boxplot
      output$boxPlot <- renderPlot({

        # dataAvg <- read_csv("pay_gap_Europe.csv", show_col_types = FALSE)
        data <- transform(data, subgroup = case_when(
          region %in% central_europe ~ "Central Europe",
          region %in% northern_europe ~ "Northern Europe",
          region %in% eastern_europe ~ "Eastern Europe",
          region %in% southern_europe ~ "Southern Europe",
          TRUE ~ "Other"
        ))

        p1 <- ggplot(data, aes(x=subgroup, y=!!sym(input$variablesOne), fill=subgroup)) + geom_boxplot() + theme(axis.title.x=element_blank(), legend.position = "none")
          
        p2 <- ggplot(data, aes(x=subgroup, y=!!sym(input$variablesTwo), fill=subgroup)) + geom_boxplot() + theme( axis.title.x=element_blank())
        grid.arrange(p1, p2, nrow = 1, widths = c(1, 1.3))
        par(mfrow=c(1,1))

      }, width = 1000, height = 700)
      
      # #show dataset
      # output$distData <- renderTable(
      #   data %>%
      #     dplyr::select(unlist(input$variables, use.names = FALSE))
      # )
}

# Run the application 
shinyApp(ui = ui, server = server)
