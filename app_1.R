library(shiny)
library(ggplot2)
library(dplyr)

jobs <- c("-", "Industry", "Business",	"Mining",	"Manufacturing", "Electricity_supply",
          "Water_supply", "Construction", "Retail trade", "Transportation",	"Accommodation",
          "Information",	"Financial", "Real estate", "Professional_scientific",
          "Administrative", "Public_administration", "Education", "Human_health",
          "Arts", "Other")

ui <- fluidPage(
  titlePanel("Average Pay Gap in Europe"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country", choices = unique(pay_gap_Europe$Country)),
      selectInput("profession1", "Profession 1", choices = jobs),
      selectInput("profession2", "Profession 2", choices = jobs),
      actionButton("compareBtn", "Compare")
    ),
    mainPanel(
      plotOutput("linePlot")
    )
  )
)

server <- function(input, output){
  
  output$linePlot <- renderPlot({
    filtered_data <- pay_gap_Europe %>% filter(Country == input$country)
    ggplot(data = filtered_data, aes(x = Year, y = Average)) +
      geom_line(aes_string(y = input$profession1), color = "darkred") +
      geom_line(aes_string(y = input$profession2), color = "blue") +
      geom_line(aes(y = Average), color = "black") +
      geom_point()
  })
  
}

shinyApp(ui, server)

# pay_gap_Europe %>%
#   filter(Country == "France") %>%
#   ggplot(data = ., aes(x = Year, y = Average)) +
#   geom_line(aes(y = Business), color = "darkred") +
#   geom_line(aes(y = Mining), color = "blue") +
#   geom_line(aes(y = Average), color = "black") +
#   geom_point()
