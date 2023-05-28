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
library(readr)

#Load datasets

## -- FOR THE MAP
data <- read_csv("pay_gap_Europe.csv", show_col_types = FALSE)
colnames(data) <- c("region","year","GDP","UrbanPopulation","Industry","Business","Mining", 
                    "Manufacturing","ElectricitySupply","WaterSupply","Construction",
                    "Retail","Transportation","Accommodation","Information",
                    "Financial","RealEstate","Science",
                    "Administrative","PublicAdministration","Education",
                    "Health","Arts","Other","Average")

## -- FOR THE MAP

worldmap <- map_data("world")
mapdata <- left_join(data, worldmap, by="region", relationship = "many-to-many")

## -- FOR THE MATRIX & LM

#create a new column with the country
data$Country_factor <- as.factor(data$region)
#give to each country a qualitative value
data$Country_numeric <- as.numeric(data$Country_factor) - 1
#remove from the past dataset the qualitative values
pay_gap<-subset(data,select=-c(region, Country_factor))

## -- FOR LINEAR GRAPH

linearPayGap<-subset(data,select=-c(Average,Country_factor,Country_numeric))

## -- FOR BOX PLOT

boxData <- subset(data,select=-c(Country_factor,Country_numeric))

## -- SET THE DIFFERENT VECTORS & LIST 

central_europe <- c("France","Luxembourg","Netherlands","Belgium","Germany", "Austria", "Czech Republic", "Hungary", "Poland", "Slovakia", "Slovenia")
northern_europe <- c("Denmark", "Estonia", "Finland", "Iceland", "Latvia", "Lithuania", "Norway", "Sweden")
eastern_europe <- c("Bulgaria", "Croatia", "Romania")
southern_europe <- c("Cyprus", "Greece", "Italy", "Malta", "Portugal", "Spain")

allcolumnNames = c(colnames(data))
selectedcolumn = c("GDP","Industry","Business","Mining", 
                   "Manufacturing","ElectricitySupply","WaterSupply","Construction",
                   "Retail","Transportation","Accommodation","Information",
                   "Financial","RealEstate","Science",
                   "Administrative","PublicAdministration","Education",
                   "Health","Arts","Other")

selectedcountries = c(unique(data$region))

variables<-pay_gap[,selectedcolumn]

# Header ----
header <- dashboardHeader(title="Gender Pay Gap")

# SideBar ----

sidebar <- dashboardSidebar(
  conditionalPanel(condition="input.tabselected==1",
                   h4(HTML("Group ID 6"),style="text-align:right"),
                   h5(HTML("<br>Presentation by 
                            <br>Marion Salaün,
                            <br>Ainhoa Unanue Bereciartu
                            <br>& Cheung Sum Yuet"),style="text-align:right" )
  ),
  conditionalPanel(condition="input.tabselected==2",
                   sliderInput("yearCursor", "Years :",
                               min = 2010, max = 2021,
                               value = 2010, step = 2, sep = "",
                               animate = TRUE),
                   selectInput("jobField", "Choose the Job field :",
                               choices=selectedcolumn),
  ),
  conditionalPanel(condition="input.tabselected==3",
                   checkboxGroupInput("country","Select Country:",
                                      choices = selectedcountries,
                                      selected = c("France","Spain")),
                   actionLink("selectall","Select All or Reset"),
  ),
  conditionalPanel(condition="input.tabselected==4",
                   selectInput("variablesOne","Select variables:",
                               choices = c(selectedcolumn,"Average"),
                               selected = c("Average"),
                               
                   ),
                   selectInput("variablesTwo","Select variables:",
                               choices = c(selectedcolumn,"Average"),
                               selected = c("UrbanPopulation"),
                               
                   ),
  ),
  conditionalPanel(condition="input.tabselected==5",
                   selectInput("norm","Normalisation:",
                               choices = c("row","column","none"),
                               selected = c("none"),
                   ),
                   selectInput("reference","Select the reference variables:",
                               choices = allcolumnNames,
                               selected = c("Information"),
                   ),
                   selectInput("predicator","Select predicators:",
                               choices = allcolumnNames,
                               selected = c("Year","GDP","Urban_Population"),
                               multiple = TRUE,
                   ),
  )
)

# Body ----
body <- dashboardBody(
  mainPanel(
    tabsetPanel(
      tabPanel("About", value=1,
               fluidRow(
                 box(width = 12, height = 600,
                     h1("First ..."),
                     div("As young women and future professionals in IT (for some of us reconverted), 
                             we wanted to see if we can be as paid as much as our future male coworkers."),
                     h2("About our database "),
                     div(HTML("These datas are from the European institution named <b>Eurostat</b>. The data
                             were collected <b>from 2010 until 2021</b> included. <b>27 European countries</b> were selected.
                             Only the employees working for a company with more than 10 employees were chosen.")),
                     h2("About the Gender Pay Gap "),
                     div(HTML("<b>Gender Pay Gap (GPG)</b> is computed as : 
                              <em>(Men's Average Earnings - Women's Average Earnings) / Men's Average Earnings</em> 
                              which means if GPG = 0.20, women earn 20% less than men in the same company.")),
                     h2("Our questions"),
                     div(HTML("<ul>
                                  <li>What was the trend in the gender pay gap between female and male workers from 2010 to 2021?</li>
                                  <li>Is there significant variation in the gender pay gap across European countries?</li>
                                  <li>Are there variations in the gender pay gap across different professional fields?</li>
                                  <li>Is there a correlation between urbanization and the gender pay gap?</li>
                                  <li>Is there a correlation between GDP and the gender pay gap?</li>
                                  <li>Do we have a situation like ‘In the southern European countries, the gap is bigger than in the northern countries.’?</li>
                                  </ul>"))
                     
                 )
               )
      ),
      tabPanel("Map", value=2,
               fluidRow(
                 box(title = "Map of Europe", height = 500,width = 6, plotOutput("distPlot")),
                 box(width = 3, height = 500, tableOutput("viewCountries1")),
                 box(width = 3, height = 500, tableOutput("viewCountries2")),
                 div(HTML("<b>Gender Pay Gap (GPG)</b> is computed as : 
                     <em>(Men's Average Earnings - Women's Average Earnings) / Men's Average Earnings</em> 
                       which means if GPG = 0.20, women earn 20% less than men in the same company."),style="text-align:center")
               )
      ),
      tabPanel("Line Plot", value=3,
               fluidRow(
                 box(title = "Business line", width = 12, plotOutput("linePlot")),
                 box(title = "Information", width = 12,
                     div(HTML("Each <b>business line </b> was regrouped :
                            <ul>
                            <li>Trade and commerce for Retail, </li>
                            <li>Manufacturing and production for manufacturing, </li>
                            <li>Primary Industry and Infrastructure for Electricity Supply, Water Supply, Mining, Construction </li>
                            <li>Service and information for Business,Transportation, Accommodation, Information, Financial, Real Estate, Science, Administrative </li>
                            <li>Public sector and social services for Health, Arts, Others, Education, Public Administration </li> 
                            </ul>
                              "))
                 )
               )
               
      ),
      tabPanel("Box plot", value=4,
               fluidRow(
                 box(title = "Urban population vs ... ", width = 12, plotOutput("boxPlot")),
                 box(title = "Information", width = 12,
                     div(HTML("Each <b>country </b> was regrouped as :
                              <ul>
                              <li>Central Europe  : France, Luxembourg , Netherlands, Belgium, Germany, Austria, Czech Republic, Hungary, Poland, Slovakia, Slovenia</li>
                              <li>Northern Europe : Denmark, Estonia, Finland, Iceland, Latvia, Lithuania, Norway, Sweden</li>
                              <li>Southern Europe : Bulgaria, Croatia, Romania</li>
                              <li>Eastern Europe : Cyprus, Greece, Italy, Malta, Portugal, Spain</li>
                              <li>Other : Switzerland </li>
                              </ul>
                                ")))
               )
               
      ),
      
      tabPanel("Correlation", value=5, 
               fluidRow(
                 box(title = "Correlation matrix ", width = 12, plotOutput("corrMatrix")),
                 box(title = "Information", width = 12,verbatimTextOutput("summaryModel")),
               )
      ),
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
    data1 <- data1 %>% distinct(.keep_all=TRUE)
    
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
  
  
  #Create the button to select/deselect all countries
  observeEvent(input$selectall,{
    if(input$selectall %% 2 == 1)
      updateCheckboxGroupInput (session,"country","Select Country:",
                                choices = selectedcountries,
                                selected = selectedcountries)
    else
      updateCheckboxGroupInput (session,"country","Select Country:",
                                choices = selectedcountries,
      )
  })
  
  #Line plot
  output$linePlot <- renderPlot({
    
    countryname <- req(input$country)
    
    plotLineData <- linearPayGap %>%
      pivot_longer(cols = -c("region","year","GDP","UrbanPopulation"), names_to = 'Domain', values_to = 'GPG')
    
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
    
  })
  
  #Correlation matrix
  output$corrMatrix <- renderPlot({
    
    #see the correlation of pay_gap
    pay_gap.corr<-cor(na.omit(pay_gap))
    
    #Make a graph for the correlation
    palette <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
    
    heatmap(x = pay_gap.corr,
            # Colv = NA,
            # Rowv = NA,
            cexRow = 0.8,
            cexCol = 0.8,
            labCol = allcolumnNames,
            labRow = allcolumnNames,
            col= palette,
            scale=input$norm, # scale allow the normalization
            symm = TRUE)
    # legend(x = "bottomright", c("Max", "Mid","Min"), fill = c("green4","White","deeppink"))
  })
  
  #Boxplot
  output$boxPlot <- renderPlot({
    
    boxData <- transform(boxData, subgroup = case_when(
      region %in% central_europe ~ "Central Europe",
      region %in% northern_europe ~ "Northern Europe",
      region %in% eastern_europe ~ "Eastern Europe",
      region %in% southern_europe ~ "Southern Europe",
      TRUE ~ "Other"
    ))
    
    p1 <- ggplot(boxData, aes(x=subgroup, y=!!sym(input$variablesOne), fill=subgroup)) + 
      geom_boxplot() + 
      theme(axis.title.x=element_blank(),axis.text.x = element_blank(), legend.position = "none")
    
    p2 <- ggplot(boxData, aes(x=subgroup, y=!!sym(input$variablesTwo), fill=subgroup)) + 
      geom_boxplot() + 
      theme(axis.title.x=element_blank(),axis.text.x = element_blank())
    
    grid.arrange(p1, p2, nrow = 1, widths = c(0.5, 0.8))
    par(mfrow=c(1,1))
  })
  
  #reactive function with LM
  scaled_variables <- as.data.frame(scale(variables))
  data <- cbind(scaled_variables, Year = pay_gap$year, Urban_population=pay_gap$UrbanPopulation,Country_numeric=pay_gap$Country_numeric)
  data <- na.omit(data)
  lm1 <- reactive({lm(reformulate(input$predicator,input$reference), data = data)})
  
  #Modelling
  output$summaryModel <- renderPrint({summary(lm1())})
  
}

# Run the application 
shinyApp(ui = ui, server = server)