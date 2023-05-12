library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

pay_gap_Europe <- read_csv("./pay_gap_Europe.csv", show_col_types = FALSE)

jobs <- c("Industry", "Business",	"Mining",	"Manufacturing", "Electricity_supply",
          "Water_supply", "Construction", "Retail trade", "Transportation",	"Accommodation",
          "Information",	"Financial", "Real estate", "Professional_scientific",
          "Administrative", "Public_administration", "Education", "Human_health",
          "Arts", "Other")

ui <- fluidPage(
  titlePanel("Average Pay Gap in Europe"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country", choices = unique(pay_gap_Europe$Country)),
      checkboxGroupInput("myCheckboxes", "Select profession(s):",
                         choices = jobs,
                         selected = c("Industry", "Business")
      ),
    ),
    mainPanel(
      plotOutput("linePlot")
    )
  )
)

server <- function(input, output) {
  output$linePlot <- renderPlot({
    filtered_data <- pay_gap_Europe %>%
      filter(Country == input$country)
    
    # Extract the selected professions from the checkbox input
    selected_professions <- input$myCheckboxes
    
    # Filter the data to include only the selected professions
    filtered_data <- filtered_data %>%
      select(Year, Average, all_of(selected_professions))
    
    # Reshape the data from wide to long format
    filtered_data_long <- filtered_data %>%
      tidyr::pivot_longer(cols = -c(Year, Average), names_to = "Profession", values_to = "Value")
    
    # Plotting the lines for selected professions
    ggplot(data = filtered_data_long, aes(x = Year, y = Value, color = Profession)) +
      geom_line() +
      geom_point() +
      scale_color_brewer(palette = "Set1") +
      theme(legend.position = "bottom")
  })
}


shinyApp(ui, server)
