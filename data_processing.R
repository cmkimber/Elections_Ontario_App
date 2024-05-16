setwd("~/Documents/Elections_Ontario_App")

library(tidyverse)
library(sf)

# when reading in the shapefile and the election results, the format of the district IDs must be harmonized (leading 0s in one and not the other)

electoral_districts <- st_read("./data/Electoral District Shapefile - 2022 General Election/ELECTORAL_DISTRICT.shp")

electoral_results_2022 <- read_csv("./data/Valid Votes Cast for Each Candidate_2024-Mar-07.csv") %>% mutate(ElectoralDistrictNumber = as.numeric(str_remove(string = ElectoralDistrictNumber, pattern = "^0+")))

# for mapping purposes, the shapefile needs the information on the winner of each district in the election attached

electoral_winners <- electoral_results_2022 %>% filter(IsGeneralElection == 1, Plurality > 0)

electoral_winners <- left_join(electoral_districts, electoral_winners, by = join_by(ED_ID == ElectoralDistrictNumber))


electoral_winners_WGS84 <- st_transform(electoral_winners, 4326)
electoral_winners_WGS84 <- electoral_winners_WGS84 %>% mutate(across(PoliticalInterestCode, as.factor))

#saveRDS(electoral_districts, file = "./app-1/data/electoral_districts.rds")
saveRDS(electoral_winners_WGS84, file = "./app-1/data/electoral_winners_WGS84.rds")
saveRDS(electoral_results_2022, file = "./app-1/data/electoral_results_2022.rds")