setwd("~/Documents/Elections_Ontario_App/elections_ontario_app")

library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(DT)
library(glue)
library(ggiraph)
library(ggrepel)

election_and_party <- readRDS("./data/election_and_party.rds")

ui <- fluidPage(
  titlePanel("Total Votes by Party"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      helpText("The graph to the right displays total votes for each party across all districts in a given general election, and the percentage of all votes in the election each party received. Parties that received less than 0.5% of all votes cast in the election are included in the category Other. Select the general election you wish to view from the dropdown menu."),
      selectInput(inputId = "year2",
                  label = "Election",
                  choices = sort(unique(election_and_party$Year), decreasing = TRUE),
                  selected = 2022)
    ),
    mainPanel = mainPanel(
      girafeOutput(outputId = "donut_plot", height = 500)
    )
  )
)

server <- function(input, output, session){
  
  panel_2_df <- reactive({
    election_and_party %>%
      filter(Year == input$year2) %>%
      arrange(desc(Pct.Votes)) %>%
      mutate(Party.Temp = factor(Party.Temp, levels = Party.Temp),
             Party.Temp = fct_relevel(Party.Temp, "Other", after = Inf)) %>%
      arrange(Party.Temp) %>%
      mutate(csum = rev(cumsum(rev(Pct.Votes))),
             pos = Pct.Votes/2 + lead(csum, 1),
             pos = if_else(is.na(pos), Pct.Votes/2, pos))
  })
  
  temp_palette <- reactive({
    panel_2_df()$Party.Col
  })
  
  output$donut_plot <- renderGirafe({
    panel_2_plot <- ggplot() +
      geom_col_interactive(data = panel_2_df(),
                           color = "white",
                           aes(x = 1,
                               y = Pct.Votes,
                               fill = Party.Temp,
                               tooltip = glue("<b>{Party.Temp}</b><br/>",
                                              "{Votes.Cast} ({round(Pct.Votes, digits = 2)}%)")),
                           show.legend = FALSE) + 
      coord_polar(theta = "y", start = 0) +
      geom_label_repel_interactive(data = panel_2_df(),
                                   aes(x = 1.2, y = pos, label = paste0(Party.Acr, "\n", Votes.Cast, " (", round(Pct.Votes, digits = 1), ")%")),
                                   size = 3.5,
                                   #box_padding = 0.5,
                                   nudge_x = 0.8,
                                   #nudge_y = 0.5,
                                   show.legend = FALSE) +
      xlim(c(0.1, 2)) + 
      scale_fill_manual(values = temp_palette()) +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())
    
    girafe(ggobj = panel_2_plot)
  })
}

shinyApp(ui = ui, server = server)