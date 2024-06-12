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

election_seat_ballot <- readRDS("./data/election_seat_ballot.rds")

# set a colour palette for each party that is robust to changes in which party is selected to display in the plot
colour_table <- election_seat_ballot %>%
  ungroup() %>%
  select(c(Party, Party.Col)) %>%
  distinct(Party, .keep_all = TRUE)
pane_3_palette <- setNames(colour_table$Party.Col, colour_table$Party)

response_options <- c("Seats" = "Seats.Won",
                      "Ballots" = "Votes.Cast",
                      "% Seats" = "Pct.Seats",
                      "% Ballots" = "Pct.Votes")

ui <- fluidPage(
  titlePanel("holder"),
  helpText("holder"),
  selectizeInput(inputId = "party",
                 label = "Parties",
                 choices = unique(election_seat_ballot$Party),
                 selected = c("Ontario Liberal Party", "Green Party of Ontario", "Progressive Conservative Party of Ontario", "New Democratic Party of Ontario", "Family Coalition Party"),
                 multiple = TRUE),
  checkboxInput(inputId = "percentage", 
                label = "Show percentage",
                FALSE),
  radioButtons(inputId = "seats_ballots",
                 label = "",
                 choices = c("Seats" = "Seats.Won",
                             "Ballots" = "Votes.Cast")),
  plotlyOutput("pane_3_plot"),
  DTOutput("pane_3_table")
)

server <- function(input, output, session){
  
  parties_to_display <- reactive({
    election_seat_ballot %>% filter(Party %in% input$party)
  })
  
  # make the hidden choices underlying the radio buttons responsive to whether total or percent values are selected
  observeEvent(input$percentage, {
    if (input$percentage == FALSE){
      updateRadioButtons(session,
                         inputId = "seats_ballots",
                         choices = c("Seats" = "Seats.Won",
                                     "Ballots" = "Votes.Cast"))
    }
    else if (input$percentage == TRUE){
      updateRadioButtons(session,
                         inputId = "seats_ballots",
                         choices = c("Seats" = "Pct.Seats",
                                     "Ballots" = "Pct.Votes"))
    }
  })
  
  output$pane_3_plot <- renderPlotly({
    # response <- switch(input$seats_ballots,
    #                    "Seats.Won" = parties_to_display()$Seats.Won,
    #                    "Votes.Cast" = parties_to_display()$Votes.Cast,
    #                    Seats.Won)
  
    pane_3_plot <- ggplot(data = parties_to_display(), aes_string(x = "Year", y = input$seats_ballots, group = "Party")) +
      geom_line(aes(color = Party)) +
      labs(x = "Year",
           y = names(response_options[which(response_options == input$seats_ballots)])) +
      scale_color_manual(values = pane_3_palette) +
      theme_light() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.position = "none")
    
    ggplotly(pane_3_plot, dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x") %>%
      style(mode = "markers+lines")
  })
  
  # Store information on which year is selected in plot in a reactive value and observe to maintain the saved year until a new year is hovered over. This avoids the year being set to NULL when moving between points and blanking the DataTable.
  chosen_year <- reactiveVal()
  observe({
    hover_data <- event_data(event = "plotly_hover")$x[1]
    if(!is.null(hover_data))
      chosen_year(hover_data)
  })
  
  output$pane_3_table <- DT::renderDataTable({
    req(!is.null(chosen_year()))
    df_pane_3_out <- parties_to_display() %>%
      select(c(Party, Year, input$seats_ballots)) %>%
      filter(Year == chosen_year())
    datatable(df_pane_3_out,
              rownames = FALSE,
              options = list(dom = "t",
                             searching = FALSE,
                             order = list(list(2, "desc")))) %>%
      formatStyle("Party",
                  backgroundColor = styleEqual(levels = c("Green Party of Ontario", "Ontario Liberal Party", "New Democratic Party of Ontario", "Progressive Conservative Party of Ontario"),
                                               values = c("#A6D43D", "#E06666", "#FFA257", "#498BE7")))
  })
}

shinyApp(ui = ui, server = server)