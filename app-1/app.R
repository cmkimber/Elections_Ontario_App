setwd("~/Documents/Elections_Ontario_App/app-1")

library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(DT)
library(glue)

electoral_winners_WGS84 <- readRDS("./data/electoral_winners_WGS84.rds")
electoral_results_2022 <- readRDS("./data/electoral_results_2022.rds")

# create a custom colour factor to assign the traditional colours to each political party (in order these are green, black, red, orange and blue)
factpal <- colorFactor(c("#A6D43D", "#6E6E6E", "#E06666", "#FFA257", "#498BE7"), electoral_winners_WGS84$PoliticalInterestCode)

# for ordering and searching purposes in the selectInput, the district ID is appended to each district's name to create an alias that is displayed in the selectInput
choices <- electoral_winners_WGS84$ElectoralDistrictNameEnglish
for (i in (1:length(choices))){
  names(choices)[i] <- paste0(electoral_winners_WGS84$ED_ID[i], " - ", electoral_winners_WGS84$ElectoralDistrictNameEnglish[i])
}

# to have the map zoom to fit each district regardless of geographical area, the bounding box of each district must be calculated
get_bounding_box <- function(polygon_row){
  bbox <- st_bbox(polygon_row$geometry)
  return(bbox)
}

ui <- fluidPage(
  titlePanel("Election Results Map"),
  helpText("To see the results of the 2022 General Election in a given riding, select the riding from the drop-down menu or click on the riding in the map. Note that you may search for a specific riding in the drop-down menu by typing its name or ID number."),
  # note selectize is used here with multiple selections enabled and a max number of selections of 1 to facilitate having the app initialize with no district selected
  selectizeInput(inputId = "district", label = "Electoral District", choices = sort(choices), multiple = TRUE, selected = NULL, options = list(maxItems = 1)),
  dataTableOutput("district_results"),
  leafletOutput("mymap")
)

server <- function(input, output, session){
  
  # set a leaflet proxy
  proxy <- leafletProxy("mymap")

  # set default reactive value from inputs to be null (allows map to load without riding selected and no table)
  district_input_filter <- reactiveVal(value = NULL)
  
  # sync from the reactive value to the inputs
  observeEvent(district_input_filter(),{
    updateSelectInput(session, inputId = "district", selected = district_input_filter())
    proxy %>% clearGroup(group = "highlighted_district")
    #proxy %>% addPolylines(stroke = TRUE, weight = 4, color = "yellow", data = filter(electoral_winners_WGS84, ElectoralDistrictNameEnglish == district_input_filter()), group = "highlighted_district")
    proxy %>% addPolygons(stroke = TRUE, weight = 4, color = "yellow", fillColor = "white", fillOpacity = 0.5, data = filter(electoral_winners_WGS84, ElectoralDistrictNameEnglish == district_input_filter()), group = "highlighted_district")
    bbox <- filter(electoral_winners_WGS84, ElectoralDistrictNameEnglish == district_input_filter()) %>% get_bounding_box()
    proxy %>% flyToBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]], lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
  })
  
  # sync from the inputs to the reactive value
  observeEvent(input$district, {
    district_input_filter(input$district)
  })
  observeEvent(input$mymap_shape_click$id, {
    district_input_filter(input$mymap_shape_click$id)
  })
  
  # table output uses req() to require a value from an input so it doesn't render on launch
  output$district_results <- DT::renderDataTable({
    req(!is.null(district_input_filter()))
    df_out <- electoral_results_2022 %>% filter(ElectoralDistrictNameEnglish == district_input_filter()) %>% select(c(PoliticalInterestCode, NameOfCandidates, TotalValidBallotsCast, PercentOfTotalValidBallotsCast))
    datatable(df_out, options = list(dom = "t", searching = FALSE, order = list(list(3, "desc"))), colnames = c("Party", "Candidate", "Votes", "%")) %>% formatPercentage("PercentOfTotalValidBallotsCast", digits = 2) %>% formatStyle("PoliticalInterestCode", backgroundColor = styleEqual(levels = levels(electoral_winners_WGS84$PoliticalInterestCode), values = c("#A6D43D", "#6E6E6E", "#E06666", "#FFA257", "#498BE7"), default = NULL))
  })
  
  #define the popup text  
  
  #popup_line1 <- paste0(electoral_winners_WGS84$ElectoralDistrictNameEnglish, " (", electoral_winners_WGS84$ED_ID, ")")
  #popup_line2 <- paste0(electoral_winners_WGS84$NameOfCandidates, " (", electoral_winners_WGS84$PoliticalInterestCode, ")")
  #popup_text <- paste(sep = "<br/>", popup_line1, popup_line2)
  
  popup_text <- glue(
    "<b>{electoral_winners_WGS84$ElectoralDistrictNameEnglish} ({electoral_winners_WGS84$ED_ID})</b><br/>",
    "{electoral_winners_WGS84$NameOfCandidates} ({electoral_winners_WGS84$PoliticalInterestCode})<br/>"
  )
  
  output$mymap <- renderLeaflet({
    leaflet(electoral_winners_WGS84) %>% addPolygons(layerId = ~electoral_winners_WGS84$ElectoralDistrictNameEnglish, highlightOptions = highlightOptions(color = "white", weight = 2, fillOpacity = 0.2), color = "black", weight = 1, fillColor = ~factpal(PoliticalInterestCode), fillOpacity = 1, popup = popup_text)
  })
}

shinyApp(ui = ui, server = server)