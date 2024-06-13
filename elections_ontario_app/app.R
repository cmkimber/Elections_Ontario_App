#setwd("~/Documents/Elections_Ontario_App/elections_ontario_app")

library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(DT)
library(glue)
library(ggiraph)
library(ggrepel)
library(plotly)

### LOAD PANEL 1 DATA, OBJECTS & FUNCTIONS ----

electoral_districts_WGS84 <- readRDS("./data/electoral_districts_WGS84.rds")
electoral_results <- readRDS("./data/electoral_results.rds")

winning_parties <- c("GPO", "IND", "LIB", "NDP", "PCP")

# create a custom colour factor to assign the traditional colours to each political party (in order these are green, black, red, orange and blue)
factpal <- colorFactor(c("#A6D43D", "#6E6E6E", "#E06666", "#FFA257", "#498BE7"), winning_parties)

# to have the map zoom to fit each district regardless of geographical area, the bounding box of each district must be calculated
get_bounding_box <- function(polygon_row){
  bbox <- st_bbox(polygon_row$geometry)
  return(bbox)
}

# to have the popups on the map respond to either select or map clicking inputs, the centroid of the selected district polygon must be calculated
get_centroid <- function(polygon_row){
  centroid <- st_coordinates(st_centroid(polygon_row))
  return(centroid)
}

# generate the text for the popups on the map based on what district is selected
get_popup_text <- function(polygon_row){
  district_name <- polygon_row$ElectoralDistrictNameEnglish
  district_id <- polygon_row$ElectoralDistrictNumber
  candidate_name <- polygon_row$NameOfCandidates
  candidate_party <- polygon_row$PoliticalInterestCode
  popup_text <- glue(
  "<b>{district_name} ({district_id})</b><br/>",
  "{candidate_name} ({candidate_party})<br/>"
  )
  return(popup_text)
}

### LOAD PANEL 2 DATA ----

election_and_party <- readRDS("./data/election_and_party.rds")

### LOAD PANEL 3 DATA AND OBJECTS ----

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

### LOAD PANEL 4 DATA ----

election_district_turnout <- readRDS(file = "./data/election_district_turnout.rds")

### BUILD UI ----

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Election Results Map",
      titlePanel("Election Results Map"),
      helpText("To see the results of a general election, first select an election you are interested in. Then, select the electoral district from the drop-down menu or click on the riding in the map. Once a district is selected, you may change the year to see how the results changed over time. Note that you may search for a specific district in the drop-down menu by typing its name or ID number."),
      selectInput(inputId = "year",
                  label = "Election",
                  choices = sort(unique(year(electoral_results$PollingDate)), decreasing = TRUE),
                  selected = NULL),
      # note selectize is used here with multiple selections enabled and a max number of selections of 1 to facilitate having the app initialize with no district selected
      selectizeInput(inputId = "district", 
                     label = "Electoral District", 
                     choices = NULL, 
                     multiple = TRUE, 
                     selected = NULL, 
                     options = list(maxItems = 1)),
      dataTableOutput("district_results"),
      leafletOutput("mymap")
    ),
    tabPanel("Total Votes by Party",
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
    ),
    tabPanel("Seats/Ballots Won by Parties",
      titlePanel("Seats/Ballots Won by Parties"),
      helpText("This visualization explores the success of parties over time. Use the dropdown menu to choose which parties you wish to display. Use the controls above the graph to switch between vote totals and legislature seat totals per party, either in absolute values or as percentages of total. Control the time period using the selector bar below the graph."),
      fluidRow(
       column(8,
              selectizeInput(inputId = "party",
                             label = "Parties",
                             choices = unique(election_seat_ballot$Party),
                             selected = c("Ontario Liberal Party", "Green Party of Ontario", "Progressive Conservative Party of Ontario", "New Democratic Party of Ontario", "Family Coalition Party"),
                             multiple = TRUE)
       ),
       column(4,
              checkboxInput(inputId = "percentage", 
                            label = "Show percentage",
                            FALSE),
              radioButtons(inputId = "seats_ballots",
                           label = "",
                           choices = c("Seats" = "Seats.Won",
                                       "Ballots" = "Votes.Cast"))
       )
      ),
      fluidRow(
       plotlyOutput("pane_3_plot"),
       DTOutput("pane_3_table")
      )
    ),
    tabPanel("Voter Turnout by Electoral District",
       titlePanel("Voter Turnout by Electoral District"),
       helpText("This graph compares the total number of registered voters to the total number of ballots cast (voter turnout), by riding, for elections from 1867 to 2022. The election of interest can be chosen using the dropdown menu."),
       selectizeInput(inputId = "year4",
                      label= "Election",
                      choices = sort(unique(election_district_turnout$Year), decreasing = TRUE),
                      selected = 2022),
       plotlyOutput("pane_4_plot")
    )
  )
)

### BUILD SERVER ----

server <- function(input, output, session){
  
  ### PANE 1 SERVER CONTENTS ----
  
  # set a leaflet proxy
  proxy <- leafletProxy("mymap")
  
  # fitler the dataset based on the year chosen and create a subset showing district winners for mapping
  electoral_year <- reactive({
    filter(electoral_results, year(PollingDate) == input$year)
  })
  
  electoral_winners <- reactive({
    electoral_results %>%
    filter(year(PollingDate) == input$year & Plurality > 0) %>%
    mutate(across(PoliticalInterestCode, as.factor))
  })

  # populate the second select input based on the year chosen in the first  
  observeEvent(electoral_year(), {
    choices <- electoral_year()$ElectoralDistrictNumber
    # for ordering and searching purposes in the selectInput, the district ID is appended to each district's name to create an alias that is displayed in the selectInput
    for (i in (1:length(choices))){
      names(choices)[i] <- paste0(electoral_year()$ElectoralDistrictNumber[i], " - ", electoral_year()$ElectoralDistrictNameEnglish[i])
    }
    #district_input_filter(NULL)
    updateSelectizeInput(session, "district",
                      choices = sort(choices),
                      selected = district_input_filter())
  })
  
  # colouring district polygons by winner
  observeEvent(electoral_year(), {
    current_map <- left_join(electoral_districts_WGS84, electoral_winners(), by = join_by(ED_ID == ElectoralDistrictNumber))
    proxy %>% clearGroup(group = "district_winners")
    proxy %>% addPolygons(layerId = ~current_map$ED_ID,
                          highlightOptions = highlightOptions(color = "white",
                                                              weight = 2,
                                                              fill = "white",
                                                              fillOpacity = 0.5),
                          color = "black",
                          weight = 1,
                          fillColor = ~factpal(PoliticalInterestCode),
                          fillOpacity = 1,
                          data = current_map,
                          group = "district_winners")
  })
  
  # set default reactive value from inputs to be null (allows map to load without riding selected and no table)
  district_input_filter <- reactiveVal(value = NULL)
  
  # sync from the reactive value to the inputs
  observeEvent(district_input_filter(),{
    updateSelectizeInput(session, inputId = "district", selected = district_input_filter())
    bbox <- filter(electoral_districts_WGS84, ED_ID == district_input_filter()) %>%
      get_bounding_box()
    proxy %>% fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]], lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
  })
  
  # sync from the inputs to the reactive value
  observeEvent(input$district, {
    district_input_filter(input$district)
  })
  observeEvent(input$mymap_shape_click$id, {
    district_input_filter(input$mymap_shape_click$id)
  })
  
  # set the popups showing the district winner and district information to respond to a change in either year or district selected, to handle cases where the user wants to see the winner of the same district for different years
  observeEvent(c(
    electoral_year(),
    district_input_filter()),
    {
      # use req to prevent error before the first district is chosen
      req(!is.null(district_input_filter()))
      proxy %>% clearGroup(group = "district_popup")
      centroid <- filter(electoral_districts_WGS84, ED_ID == district_input_filter()) %>% 
        get_centroid()
      popup_text <- filter(electoral_winners(), ElectoralDistrictNumber == district_input_filter()) %>%
        get_popup_text()
      proxy %>% addPopups(lng = centroid[1,1],
                          lat = centroid[1,2],
                          popup = popup_text,
                          data = filter(electoral_districts_WGS84, ED_ID == district_input_filter()),
                          group = "district_popup")
      # make highlighted district persist when selected year changes
      proxy %>% clearGroup(group = "highlighted_district")
      proxy %>% addPolygons(stroke = TRUE,
                            weight = 4,
                            color = "yellow",
                            fillColor = "white",
                            fillOpacity = 0.5,
                            data = filter(electoral_districts_WGS84, ED_ID == district_input_filter()),
                            group = "highlighted_district")
    }
  )
  
  # table output uses req() to require a value from an input so it doesn't render on launch
  output$district_results <- DT::renderDataTable({
    req(!is.null(district_input_filter()))
    df_out <- electoral_year() %>%
      filter(ElectoralDistrictNumber == district_input_filter()) %>%
      select(c(PoliticalInterestCode, NameOfCandidates, TotalValidBallotsCast, PercentOfTotalValidBallotsCast))
    datatable(df_out,
              options = list(dom = "t",
                             searching = FALSE,
                             order = list(list(3, "desc"))),
              colnames = c("Party", "Candidate", "Votes", "%")) %>%
      formatPercentage("PercentOfTotalValidBallotsCast", digits = 2) %>%
      formatStyle("PoliticalInterestCode",
                  backgroundColor = styleEqual(levels = winning_parties,
                                               values = c("#A6D43D", "#6E6E6E", "#E06666", "#FFA257", "#498BE7"),
                                               default = NULL))
  })
  
  output$mymap <- renderLeaflet({
    leaflet(electoral_districts_WGS84) %>%
      addPolygons(color = "black",
                  weight = 0,
                  fillColor = NULL,
                  fillOpacity = 0)
  })
  
  ### PANE 2 SERVER CONTENTS ----
  
  panel_2_df <- reactive({
    election_and_party %>%
      filter(Year == input$year2) %>%
      arrange(desc(Pct.Votes)) %>%
      mutate(Party.Temp = factor(Party.Temp, levels = Party.Temp)) %>%
      mutate(Party.Temp = if ("Other" %in% levels(Party.Temp)) fct_relevel(Party.Temp, "Other", after = Inf) else Party.Temp) %>%
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
  
### PANE 3 SERVER CONTENTS ----
  
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
    pane_3_plot <- ggplot(data = parties_to_display(),
                          aes(x = Year,
                              y = .data[[input$seats_ballots]],
                              group = Party,
                              text = paste(Party, Year, .data[[input$seats_ballots]], sep = "\n"))) +
      geom_line(aes(color = Party)) +
      labs(x = "Year",
           y = names(response_options[which(response_options == input$seats_ballots)])) +
      scale_color_manual(values = pane_3_palette) +
      theme_light() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.position = "none")
    
    ggplotly(pane_3_plot, dynamicTicks = TRUE, tooltip = "text") %>%
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
                             order = list(list(2, "desc"))),
              colnames = c("Party", "Year", names(response_options[which(response_options == input$seats_ballots)]))) %>%
      formatStyle("Party",
                  backgroundColor = styleEqual(levels = c("Green Party of Ontario", "Ontario Liberal Party", "New Democratic Party of Ontario", "Progressive Conservative Party of Ontario"),
                                               values = c("#A6D43D", "#E06666", "#FFA257", "#498BE7")))
  })

### PANE 4 SERVER CONTENTS ----

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
                           hovertemplate = paste("%{y:,d} (%{text:.2%})"),
                           texttemplate = "%{text:.0%}",
                           textposition = "outside") %>%
      add_trace(y = ~Registered.Voters,
                text = ~Registered.Voters,
                name = "Registered Voters",
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