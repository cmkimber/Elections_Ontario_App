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

election_district_turnout <- readRDS(file = "./data/election_district_turnout.rds")

ui <- fluidPage(
  titlePanel("holder"),
  helpText("holder"),
  selectizeInput(inputId = "year4",
                 label= "Election",
                 choices = sort(unique(election_district_turnout$Year), decreasing = TRUE),
                 selected = 2022),
  plotlyOutput("pane_4_plot")
)

server <- function(input, output, session){
  
  election_to_display <- reactive({
    election_turnout_district %>%
      filter(Year == input$year4) %>%
      select(c(Electoral.District, Registered.Voters, Voter.Turnout, Pct.Turnout)) %>%
      pivot_longer(-Electoral.District)
  })
  
  output$pane_4_plot <- renderPlotly({
    pane_4_plot <- ggplot(data = filter(election_to_display(), value != "Pct.Turnout"), aes(x = Electoral.District, y = value, fill = name)) + 
      geom_col(position = "dodge")
    
    ggplotly(pane_4_plot, dynamicTicks = TRUE) %>%
      rangeslider()
    
  })
}

shinyApp(ui = ui, server = server)