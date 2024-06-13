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
  titlePanel("Historical Voter Turnout"),
  helpText("This graph shows the total number of registered voters and actual voter turnout for each election from 1867 to 2022. To change the time period shown, use the selector below the plot."),
  plotlyOutput("pane_5_plot")
)

server <- function(input, output, session){
  
  output$pane_5_plot <- renderPlotly({
    pane_5_plot <- plot_ly(election_results,
                           x = ~Year) %>%
      add_trace(y = ~Registered.Voters,
                name = "Total Voters",
                mode = "lines",
                fill = "none",
                line = list(width = 2,
                            color = "#0000006b",
                            dash = "dash"),
                hovertemplate = paste("%{y:,d}")) %>%
      add_trace(y = ~Voter.Turnout,
                text = ~Pct.Turnout,
                name = "Voter Turnout",
                mode = "lines+markers+text",
                fill = 'tozeroy',
                line = list(width = 0.75,
                           color = "#0000006b"),
                marker = list(color = "#0000006b"),
                fillcolor = "#ffcf00",
                textfont = list(size = 8),
                hovertemplate = paste("%{y:,d} (%{text:.2%})"),
                texttemplate = "%{text:.0%}",
                textposition = "top") %>%
      rangeslider(start = 1867, end = 1940) %>%
      layout(xaxis = list(title = "Years",
                          ticks = "outside"),
             yaxis = list(title = "Ballots"),
             hovermode = "x unified")
      
  })
  
}

shinyApp(ui = ui, server = server)