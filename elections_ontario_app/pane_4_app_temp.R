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
  titlePanel("Voter Turnout by Electoral District"),
  helpText("This graph compares the total number of registered voters to the total number of ballots cast (voter turnout), by riding, for elections from 1867 to 2022. The election of interest can be chosen using the dropdown menu."),
  selectizeInput(inputId = "year4",
                 label= "Election",
                 choices = sort(unique(election_district_turnout$Year), decreasing = TRUE),
                 selected = 2022),
  plotlyOutput("pane_4_plot")
)

server <- function(input, output, session){
  
  election_to_display <- reactive({
    election_district_turnout %>%
      filter(Year == input$year4) %>%
      select(c(Electoral.District, Registered.Voters, Voter.Turnout, Pct.Turnout))
  })
  
  output$pane_4_plot <- renderPlotly({
    
  # Build this plot using plot_ly() because setting default limits for rangeslider() does not work with ggplotly()
    pane_4_plot <- plot_ly(election_to_display(),
                           x = ~Electoral.District,
                           y = ~Voter.Turnout,
                           type = "bar",
                           text = ~Pct.Turnout,
                           name = "Voter Turnout",
                           marker = list(color = "#ffcf00"),
                           hovertemplate = paste("%{y:,d} (%{text:.2%})"),
                           texttemplate = "%{text:.0%}",
                           textposition = "outside") %>%
      add_trace(y = ~Registered.Voters,
                text = ~Registered.Voters,
                name = "Registered Voters",
                marker = list(color = "#0000006b"),
                hovertemplate = paste("%{y:,d}"),
                texttemplate = "%{y:,d}",
                textposition = "outside") %>%
      # note rangeslider start for categorical seems to be indexed strangely
      rangeslider(start = -1, end = 20) %>%
      layout(xaxis = list(title = "Electoral Districts",
                          tickangle = 20,
                          ticks = "outside"),
             yaxis = list(title = "Voters",
                          nticks = 8,
                          tickformat = ",d"),
             barmode = "group",
             hovermode = "x unified")

  })
}

shinyApp(ui = ui, server = server)