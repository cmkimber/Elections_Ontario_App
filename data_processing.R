setwd("~/Documents/Elections_Ontario_App")

library(tidyverse)
library(lubridate)
library(sf)

# ----- Pane 1 (Map) Data Import -----

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

# ----- Pane 2 (Votes by Party Piechart) Data Import -----

election_and_party <- read_csv("./data/Votes Cast By Election and Party.csv", name_repair = "universal")
election_and_party <- election_and_party %>%
  select(-...8) %>%
  mutate(across(Election.Date, mdy))

# the data available from Elections Ontario lacks party acronyms and the official party names (and acronyms) change frequently as well, so the information on acronyms and colours associated with party had to be annotated manually. This was QUITE tedious!

# Add acronyms for parties with >0.5% vote share by election 
Party.Acr <- c("CON", "IND", "L", "Other", "CON", "CL", "IND", "L", "Other", "CON", "CI", "CLR", "IND", "L", "LI", "CON", "CI", "IND", "L", "CON", "CI" ,"IND", "LAB", "L", "LI", "CON", "IND", "LAB", "L", "Other", "CON", "CER", "ER", "L", "LER", "Other", "---", "CON", "CIP", "CP", "CPP", "IND", "L", "LOI", "LPP", "Other", "PI", "PPA", "CON", "IND", "L", "Other", "CON", "IND", "L", "Other", "PRH", "CON", "L", "LI", "Other", "CON", "CI", "IND", "LAB", "L", "Other", "S", "CON", "CI", "IND", "LAB", "L", "Other", "S", "CON", "IND", "LAB", "L", "LT", "Other", "S", "TEM", "CON", "CI", "IND", "LAB", "LUF", "L", "LIF", "Other", "SOL", "SLB", "UF", "CON", "IND", "LAB", "L", "LI", "PRO", "UF", "CON", "CI", "LAB", "L", "LI", "LPG", "LPH", "Other", "PRO", "PRI", "PRH", "UF", "CON", "CI", "LAB", "L", "LPG", "Other", "PRO", "PRH", "UF", "CCF", "C", "CON", "IND", "L", "LI", "LPG", "Other", "UF", "CCF", "CON", "CI", "FL", "LP", "L", "LI", "LPG", "Other", "CCF", "LP", "L", "LI", "Other", "PC", "CCF", "IND", "LP", "L", "LL", "LPG", "Other", "PC", "CCF", "LP", "L", "Other", "PC", "UOE", "CCF", "LP", "L", "Other", "PC", "CCF", "IND", "LP", "L", "Other", "PC", "CCF", "L", "Other", "PC", "L", "ND", "Other", "PC", "L", "ND", "Other", "PC", "I", "L", "NDP", "Other", "PC", "L", "NDP", "Other", "PC", "L", "NDP", "Other", "PC", "L", "ND", "Other", "PC", "IND", "ND", "L", "Other", "PC", "FCP", "ND", "L", "Other", "PC", "COR", "FCP", "GPO", "LIB", "LTN", "NDP", "Other", "PCP", "FCP", "IND", "LIB", "NDP", "Other", "PCP", "FCP", "GPO", "IND", "NDP", "LIB", "Other", "PCP", "FCP", "GPO", "LIB", "NDP", "Other", "PCP", "FCP", "NDP", "LIB", "Other", "PCP", "GPO", "GPO", "NDP", "LIB", "Other", "PCP", "NDP", "LIB", "LTN", "Other", "PCP", "GPO", "GPO", "NDP", "LIB", "LTN", "Other", "PCP", "GPO", "IND", "NBO", "NDP", "LIB", "ONP", "Other", "PCP")

# Add colours for parties with >0.5% vote share by election 
Party.Col <- c("#498BE7", "magenta", "#E06666", "black", "#498BE7", "black", "magenta", "#E06666", "black", "#498BE7", "black", "black", "magenta", "#E06666", "black", "#498BE7", "black", "magenta", "#E06666", "#498BE7", "black", "magenta", "black", "#E06666", "black", "#498BE7", "magenta", "black", "#E06666", "black", "#498BE7", "black", "black", "#E06666", "black", "black", "black", "#498BE7", "black", "black", "black", "magenta", "#E06666", "black", "black", "black", "black", "black", "#498BE7", 'magenta', "#E06666", "black", "#498BE7", "magenta", "#E06666", "black", "black", "#498BE7", "#E06666", "black", "black", "#498BE7", "black", "magenta", "black", "#E06666", "black", "black", "#498BE7", "black", "magenta", "black", "#E06666", "black", "black", "#498BE7", "magenta", "black", "#E06666", "black", "black", "black", "black", "#498BE7", "black", "magenta", "black", "black", "#E06666", "black", "black", "black", "black", "black", "#498BE7", "magenta", "black", "#E06666", "black", "black", "black", "#498BE7", "black", "black", "#E06666", "black", "black", "black", "black", "black", "black", "black", "black", "#498BE7", "black", "black", "#E06666", "black", "black", "black", "black", "black", "black", "black", "#498BE7", "magenta", "#E06666", "black", "black", "black", "black", "#FFA257", "#498BE7", "black", "black", "black", "#E06666", "black", "black", "black", "#FFA257", "black", "#E06666", "black", "black", "#498BE7", "#FFA257", "magenta", "black", "#E06666", "black", "black", "black", "#498BE7", "#FFA257", "black", "#E06666", "black", "#498BE7", "black", "#FFA257", "black", "#E06666", "black", "#498BE7", "black", "magenta", "black", "#E06666", "black", "#498BE7", "black", "#E06666", "black", "#498BE7", "#E06666", "#FFA257", "black", "#498BE7", "#E06666", "#FFA257", "black", "#498BE7", "magenta", "#E06666", "#FFA257", "black", "#498BE7", "#E06666", "#FFA257", "black", "#498BE7", "#E06666", "#FFA257", "black", "#498BE7", "#E06666", "#FFA257", "black", "#498BE7", "magenta", "#FFA257", "#E06666", "black", "#498BE7", "black", "#FFA257", "#E06666", "black", "#498BE7", "black", "black", "green", "#E06666", "black", "#FFA257", "black", "#498BE7", "black", "magenta", "#E06666", "#FFA257", "black", "#498BE7", 'black', "green", "magenta", "#FFA257", "#E06666", "black", "#498BE7", "black", "green", "#E06666", "#FFA257", "black", "#498BE7", "black", "#FFA257", "#E06666", "black", "#498BE7", "green", "green", "#FFA257", "#E06666", "black", "#498BE7", "#FFA257", "#E06666", "black", "black", "#498BE7", "green", "green", "#FFA257", "#E06666", "black", "black", "#498BE7", "green", "magenta", "black", "#FFA257", "#E06666", "black", "black", "#498BE7")

election_and_party <- election_and_party %>%
  group_by(Year) %>%
  mutate(Pct.Votes = Votes.Cast/sum(Votes.Cast)*100) %>%
  mutate(Party.Temp = if_else(Pct.Votes <= 0.5, "Other", Party)) %>%
  group_by(Year, Party.Temp) %>%
  summarize(Votes.Cast = sum(Votes.Cast), Pct.Votes = sum(Pct.Votes)) %>%
  add_column(Party.Acr = Party.Acr, Party.Col = Party.Col)

saveRDS(election_and_party, file = "./elections_ontario_app/data/election_and_party.rds")
