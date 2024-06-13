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
  titlePanel("holder"),
  helpText("holder"),
  plotlyOutput("pane_5_plot")
)

server <- function(input, output, session){
  
  output$pane_5_plot <- renderPlotly({
    pane_5_plot <- plot_ly(election_results,
                           x = ~Year,
                           y = ~Voter.Turnout,
                           type = "scatter",
                           mode = "lines", 
                           fill = 'tozeroy') %>%
      add_trace(x = ~Year,
                y = ~Registered.Voters,
                fill = "none",
                line = list(dash = "dash")) %>%
      rangeslider(start = 1867, end = 1940)
      
  })
  
}

shinyApp(ui = ui, server = server)