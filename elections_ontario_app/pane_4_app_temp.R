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
      select(c(Electoral.District, Registered.Voters, Voter.Turnout, Pct.Turnout))
  })
  
  output$pane_4_plot <- renderPlotly({
  #   pane_4_plot <- ggplot(data = filter(election_to_display(), value != "Pct.Turnout"), aes(x = Electoral.District, y = value, fill = name)) +
  #     geom_col(position = "dodge")
  # 
  #   ggplotly(pane_4_plot, dynamicTicks = TRUE) %>%
  #     rangeslider(start = 1, end = 20) %>%
  #     layout(hovermode = "x")
    
  # Build this plot in using plot_ly() because setting limits for rangeslider() does not work with ggplotly()
    pane_4_plot <- plot_ly(election_to_display(),
                           x = ~Electoral.District,
                           y = ~Voter.Turnout,
                           type = "bar",
                           text = ~Pct.Turnout,
                           name = "Voter Turnout",
                           hovertemplate = paste("%{y} (%{text:.2%})"),
                           texttemplate = "%{text:.0%}",
                           textposition = "outside") %>%
      add_trace(y = ~Registered.Voters,
                text = ~Registered.Voters,
                name = "Registered Voters",
                hovertemplate = paste("%{y}"),
                texttemplate = "%{y}",
                textposition = "outside") %>%
      # note rangeslider start for categorical seems to be indexed strangely
      rangeslider(start = -1, end = 20) %>%
      layout(xaxis = list(title = "Electoral Districts",
                          tickangle = 20),
             yaxis = list(title = "Voters"),
             barmode = "group",
             hovermode = "x unified")

  })
}

shinyApp(ui = ui, server = server)