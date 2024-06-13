setwd("~/Documents/Elections_Ontario_App/elections_ontario_app")

library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(DT)
library(glue)
library(ggiraph)
library(ggrepel)
library(plotly)

election_results <- readRDS("./data/election_turnout.rds")

ui <- fluidPage(
  
)

server <- function(input, output, session){
  
}

shinyApp(ui = ui, server = server)