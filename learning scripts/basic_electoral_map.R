# FIRST EXPLORATION OF HOW VARIOUS MAPPING PACKAGES WORK IN R

setwd("~/Documents/Elections Ontario Data")

library(tidyverse)
library(sf)

electoral_districts <- st_read("./data/Electoral District Shapefile - 2022 General Election/ELECTORAL_DISTRICT.shp")

plot(st_geometry(electoral_districts))

electoral_districts
  
electoral_dataset <- read_csv("./data/Valid Votes Cast for Each Candidate_2024-Mar-07.csv")

electoral_winners <- electoral_dataset %>% filter(IsGeneralElection == 1, Plurality > 0) %>% mutate(ElectoralDistrictNumber = as.numeric(str_remove(string = ElectoralDistrictNumber, pattern = "^0+")))

electoral_districts <- left_join(electoral_districts, electoral_winners, by = join_by(ED_ID == ElectoralDistrictNumber))

## ggplot can take a vector of categories as well as a vector of colours, making it easy to match the two
winning_parties <- unique(electoral_winners$PoliticalInterestCode)

scale_fill_winners <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(c('blue', 'orange', 'red', 'black', 'green'), winning_parties),
    na.value = "white",
    ...
  )
}

ggplot() + geom_sf(data = electoral_districts, aes(fill = PoliticalInterestCode)) + scale_fill_winners()

library(tmap)

## tmap does not allow the specification of an ordered categorical vector, so categories are listed alphabetically and colours must match that order

party_palette <- c('green', 'black', 'red', 'orange', 'blue')

tm_shape(electoral_districts) + tm_fill(col = "PoliticalInterestCode", palette = party_palette) +
  tm_borders()

library(leaflet)

electoral_districts_WGS84 <- st_transform(electoral_districts, 4326)
electoral_districts_WGS84 <- electoral_districts_WGS84 %>% mutate(across(PoliticalInterestCode, as.factor))

levels(electoral_districts_WGS84$PoliticalInterestCode)
factpal <- colorFactor(c("green", "black", "red", "orange", "blue"), electoral_districts_WGS84$PoliticalInterestCode)

popup_line1 <- paste0(electoral_districts_WGS84$ElectoralDistrictNameEnglish, " (", electoral_districts_WGS84$ED_ID, ")")
popup_line2 <- paste0(electoral_districts_WGS84$NameOfCandidates, " (", electoral_districts_WGS84$PoliticalInterestCode, ")")
popup_text <- paste(sep = "<br/>", popup_line1, popup_line2)

leaflet(electoral_districts_WGS84) %>% addPolygons(color = "black", weight = 1, fillColor = ~factpal(PoliticalInterestCode), fillOpacity = 0.6, popup = popup_text)
