setwd("~/Documents/Elections_Ontario_App/elections_ontario_app")

library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(DT)
library(glue)

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

ui <- fluidPage(
  titlePanel("Election Results Map"),
  helpText("To see the results of the 2022 General Election in a given riding, select the riding from the drop-down menu or click on the riding in the map. Note that you may search for a specific riding in the drop-down menu by typing its name or ID number."),
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
)

server <- function(input, output, session){
  
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
}

shinyApp(ui = ui, server = server)