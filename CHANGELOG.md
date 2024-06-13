# Changelog

## v1.3.1

### Added or Changed
- Removed the unnecssary row indices from the table displaying district data in pane 1

## v1.3.0

### Added or Changed
- Added new pane to the application that plots the total number of seats or votes obtained per party over each election from 1990-2002 using a line plot
  - Multiple parties can be displayed at once using a dropdown input and the response variable is controllable via button and checkbox inputs
- To make this plot, data obtained via Elections Ontario's Data Explorer was augmented by manual annotation to standardize official party names and add colour coding
  - These annotations were made programatically but required manual inspection to determine which party names were actually aliases of one another

## v1.2.1

### Added or Changed
- Fixed green colour on pane 2 donut plot to match that of the pane 1 map

## v1.2.0

### Added or Changed
- Added a new pane to the application that plots votes per party and percentage of total vote share obtained by each party for every election from 1867-2022
  - The election displayed is controllable via a dropdown menu
- To make this plot, data obtained via Elections Ontario's Data Explorer was augmented with manual annotation to add party acronyms and colour coding
  - These annotations were not automated due to frequent changes of official party names/acronyms over time

## v1.1.0

### Added or Changed
- Added support for data from 2018 general election
  - Note that earlier elections are currently not supported because electoral district divisions differ and shapefiles are only available for the current divisions
- Created a dropdown menu to switch between 2018 and 2022 elections
- Restructured how data on election results is stored and retrieved to facilitate multiple years
- Changed handling of on-click map popups and highlighting of selected districts to make it possible to switch between election years while keeping the same district selected

## v1.0.1

### Added or Changed
- Map popup displaying district information and winning candidate now responds to either clicking on map or selecting district from the dropdown menu

## v1.0.0

### Added or Changed
- Generated Leaflet map using Elections Ontario shapefiles for 2022 electoral districts
- Colour-coded map to represent the winning political party of each district
- Added on-click popup to map displaying district information and winning candidate
- Created dropdown input to select district using an alias to show district ID number as well as name
- Added zoom-to-district feature to map to focus on selected district
- Generated tables showing the vote share for each candidate in a selected district using Data Table, with color-coding by party to match map
- Used `eventObserve()` to sync the selected district between the dropdown and map inputs to provide the user with a seamless choice
