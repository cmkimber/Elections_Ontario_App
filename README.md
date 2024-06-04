# Elections Ontario Webapp

## About The Project

The main purpose of this project is for me to learn how to build interactive web applications or dashboards with R. To do so, I set myself the target of roughly replicating the interactive elements of the Elections Ontario's [Graphics & Charts portal](https://results.elections.on.ca/en/graphics-charts) using Shiny.

The data used is publically available and hosted by [Elections Ontario](https://results.elections.on.ca/en/publications). The shapefiles for electoral districts are governed by the Open Use Data Product License Agreement. I have not republished the data in this repo. The scripts used for ingesting and formatting data are provided, however.

This app will be built in stages with new main features and additions to existing features added regularly. [The most up-to-date functioning app is available to use here](http://cmkimber.shinyapps.io/elections_ontario_app).

## Built with

* R 4.1
* Shiny
* Leaflet
* Data Table

_Note_: In my experience, the _renderLeaflet()_ function in Shiny did not play nicely with R 4.3 at the outset of this project. The reason appears related to the change in the handling of the _&&_ operator with vectors rather than scalars in R as of 4.3. This change is covered [here](https://www.jumpingrivers.com/blog/whats-new-r43/). It is for this reason that a legacy version of R is used instead.

## Roadmap

- [x] Build MVP of Election Results Map
- [x] Add multi-year data to map pane
- [ ] Bulid additional panes
  - [ ] Total Votes by Party
  - [ ] Seats/Ballots Won by Party
  - [ ] Voter Turnout by Electoral District
  - [ ] Historical Voter Turnout

See [Open Issues](https://github.com/cmkimber/Elections_Ontario_App/issues?q=is%3Aopen+is%3Aissue) for more detail on proposed features (and known issues).

## Acknowledgements

* [Elections Ontario data portal](https://results.elections.on.ca/en/results-overview)
* [Elections Ontario Open Use Data Product License Agreement](https://www.elections.on.ca/en/voting-in-ontario/electoral-district-shapefiles/open-use-data-product-licence-agreement.html)
* [shinyapps.io](https://www.shinyapps.io/)
* [Mastering Shiny](https://mastering-shiny.org/index.html)
* [Happy Git and GitHub for the useR](https://happygitwithr.com/)
