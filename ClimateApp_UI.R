library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(plotly)
climate_data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
# Year variable for slider input
year <- c(min(climate_data$year),
          max(climate_data$year))

filtered_countries <- unique(climate_data$country)

## First Page content
page_one <- tabPanel(
  "Intro",
  titlePanel("Introduction & Summary Values"),
  p("For this Assignment, we are asked to look at a climate dataset provided by",
    a("Our World In Data", href = "https://ourworldindata.org/co2-and-other-greenhouse-gas-emissions"),
    "to explore data about CO2 emissions. With this, we are asked to choose variables in the dataset
  to analyze and i have decided to look at the cumulative co2 emissions, co2 emissions per capita, 
  and co2 emissions from different fueltypes production. Additionally, i focused primarily
  on the lastest year in the dataset which is 2019 (which was also the second 
  warmest year on record), and the United States."),
  
  p("When analyzing these values, i found that the average co2 emissions across all countries in 
    2019 is", avg_co2,". In the data set, the co2 emissions is measured in million tonnes,so
    equivalently, that is 36 billion metric tons. Knowing this, its important to note that there are
    195 countries in the world and the U.S accounts for roughly", us_co2,", or 5.2 billon metric tons.
    This is a large proportion of the total co2 emitted in 2019 and although we arent
    the biggest contributor, we are still a big contributor. Furthermore, looking at the total accumulated 
    co2 emissions to date, the world has emitted",highest_cumulative,", or 1.6 Trillion metric tons, while 
    the U.S to date has emitted", total_us_co2," or 410 billion metric tons.That is a quarter of the 
    worlds total co2 emissions and today we hold the highest rate of carbon footprint per person,
    at", capita_co2,"tons per person."),
  p("Climate change is said to affect countries on every continent, disrupting 
    economies and affecting lives through harsher weather patterns. Although many 
    governments around the world have spent considerable time and effort to plan 
    a more sustainable future, collective effort has not been maximized. Its important
    to get a grasp on the plans now to address climate change, especially before the Covid-19 pandemic
    ends, and where emissions are expected to return to higher levels.")
  
)
## Second Page content

page_two <- tabPanel(
  "Chart", 
  titlePanel("Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "country", label = "Country to View",
                  choices = filtered_countries, selected = "United States"),
      
      sliderInput(inputId = "year", label = "Year Range", min = year[1], max = year[2],
                  value = year, sep = ""),
      
      radioButtons(inputId = "fueltype", label = ("Fuel Type"), 
                   choices = list("Cement" = "cement_co2" , "Coal" ="coal_co2" ,
                                  "Flaring" = "flaring_co2",
                                  "Oil" = "oil_co2","Gas" = "gas_co2", "Other Industry" =
                                    "other_industry_co2"),
                   selected = "oil_co2")),
    mainPanel(
      # line chart of co2 emissions by different fuel type over time
      plotlyOutput(outputId = "linechart"),
      p("In this interactive chart you are able to filter for whichever country you would like to view and 
       its contribution of co2 emissions by different fueltypes. In using this, you will notice that coal, oil and gas are
       the top contributors to co2 emissions. Also, when looking at the top 3 countries that emit the most
       co2; which are China, the U.S, and India, you will notice that the Unites States emissions by Coal; which is the 
       worst energy source out of the three, is gradually decreasing the past 15 years, whereas China and India continue to gradually increase.
       This is good as it shows the U.S is doing their part in the slightest way, by moving to natural gas and renewable energy
       to reduce greenhouse emissions and protect public health. China and India and the following top co2 contributing
       countries may need to follow lead, at least in this specific case.")
    ) 
  )
)

# UI 
ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage("Data Applications(Climate Change)",
                           page_one,
                           page_two
                ))




