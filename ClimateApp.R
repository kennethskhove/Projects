library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(plotly)

climate_data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

source("app_server.R")
source("app_ui.R")



shinyApp(ui = ui, server = server)