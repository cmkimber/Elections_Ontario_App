setwd("~/Documents/Elections_Ontario_App")

library(tidyverse)
library(lubridate)
library(sf)

# when reading in the shapefile and the election results, the format of the district IDs must be harmonized (leading 0s in one and not the other)

electoral_districts <- st_read("./data/Electoral District Shapefile - 2022 General Election/ELECTORAL_DISTRICT.shp")

electoral_districts_WGS84 <- st_transform(electoral_districts, 4326)

electoral_results_2022 <- read_csv("./data/Valid Votes Cast for Each Candidate_2022.csv")

electoral_results_2018 <- read_csv("./data/Valid Votes Cast for Each Candidate_2018.csv")

electoral_results <- union_all(electoral_results_2018, electoral_results_2022) %>%
  mutate(ElectoralDistrictNumber = as.numeric(str_remove(string = ElectoralDistrictNumber, pattern = "^0+"))) %>%
  mutate(across(PollingDate, as_date)) %>%
  mutate(across(NameOfCandidates, str_to_upper)) %>%
  filter(IsGeneralElection == 1)

saveRDS(electoral_districts_WGS84, file = "./elections_ontario_app/data/electoral_districts_WGS84.rds")
saveRDS(electoral_results, file = "./elections_ontario_app/data/electoral_results.rds")