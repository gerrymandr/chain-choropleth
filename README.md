This R Shiny app allows you to visualize a choropleth map of district flips. It gives a better visual understanding of how the random walk is moving around the dual graph of voter tabulation districts. 

To run the app on your own computer, you'll need `shiny`, `leaflet`, `leaflet.extras`, `ggplot2`, `dplyr`, `sf`, `shinythemes`, and `ggthemes`. `extrafont` is optional. 

First, install `pacman` and that will load the rest of them. 

You can run it locally with `shiny::runGithub("chain-choropleth", "gerrymandr")`

The app currently supports Missouri, but extensions are forthcoming. 
