# Changelog

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
