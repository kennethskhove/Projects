library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(plotly)
# Load in climate dataset
climate_data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

# Summary 1:
# What is the average co2 emissions across all countries in 2019?
avg_co2 <- climate_data %>% 
  filter(year == "2019") %>% 
  filter(country == "World") %>% 
  pull(co2)

# Summary 2:
# What was the worlds cumulative co2 emissions to date?
highest_cumulative <- climate_data %>% 
  filter(year == max(year)) %>% 
  filter(country == "World") %>% 
  filter(cumulative_co2 == max(cumulative_co2, na.rm = T)) %>% 
  pull(cumulative_co2)

# Summary 3:
# which country emitted the lowest co2 emission in 2019? 
lowest_co2_emission <- climate_data %>% 
  filter(year == "2019") %>% 
  filter(co2 == min(co2)) %>% 
  pull(country)

# Summary 4:
# Which country has the highest co2 emission in 2019?
highest_co2_emission <- climate_data %>% 
  filter(year == max(year), country != "World", country != "Asia" ) %>% 
  filter(co2 == max(co2)) %>% 
  pull(country)
# Summary 5:
# How much co2 has the U.S emitted to date?
total_us_co2 <- climate_data %>% 
  filter(country == "United States") %>%
  filter(year == max(year)) %>% 
  pull(cumulative_co2)
# Summary 6: What is the average co2 emission per capita in the US? (in Tonnes)
capita_co2 <- climate_data %>% 
  filter(year == max(year)) %>% 
  filter(country == "United States") %>% 
  pull(co2_per_capita)

us_co2 <- climate_data %>% 
  filter(country == "United States") %>% 
  filter(year == "2019") %>% 
  pull(co2)


# Server for Application ----------------------------------------------------------

server <- function(input, output){
  output$linechart <- renderPlotly({
    
    wanted_data <- climate_data %>%
      filter(year >= input$year[1] &
               year <= input$year[2]) %>% 
      filter(country == input$country)
    
    my_plot <- ggplot(data = wanted_data)+
      geom_line(mapping = aes_string(x = "year", y = input$fueltype))+
      labs(title = paste("Co2 Emissions by Fuel Type"), x = "Year", 
           y = paste(input$fueltype, "measured in Million Tonnes"))+
      scale_y_continuous(limits = c(0,3000))
    ggplotly(my_plot)
    
    
  })
}