library(ggplot2)
library(dplyr)
library(sf)
library(geojsonio)
library(jsonlite)
library(leaflet.extras)
library(leaflet)
library(readr)

mo_flips <- read_csv("mo_flips.csv")

geojson_read_sf <- function(url, epsg = 26915) {
  geojson_read(url, what = "sp") %>% # read in as sp
    st_as_sf() %>% # convert to sf
    st_transform(epsg) # UTM zone 15n
}

mo_vtd_sf <- geojson_read_sf("mo_dists.geojson")

mo_flips_sf <- mo_vtd_sf %>%
  left_join(
    mo_flips %>%
      select(GEOID10, nodeFlips)
  )

url <- "https://www2.census.gov/geo/tiger/TIGER2013/CD/tl_2013_us_cd113.zip"
tmpdir <- tempdir()
file <- basename(url)
download.file(url, file)
unzip(file, exdir = tmpdir)
cd_2013 <- st_read("C:\\Users\\katie\\AppData\\Local\\Temp\\RtmpwnRvry","tl_2013_us_cd113")

cd_2013_mo <- cd_2013 %>%
  filter(STATEFP == "29") %>%
  st_transform("+init=epsg:4326")


url <- "https://www2.census.gov/geo/tiger/TIGER2013/STATE/tl_2013_us_state.zip"

tmpdir <- tempdir()
file <- basename(url)
download.file(url, file)
unzip(file, exdir = tmpdir)
state_2013 <- st_read("C:\\Users\\katie\\AppData\\Local\\Temp\\RtmpwnRvry","tl_2013_us_state")

state_2013_mo <- state_2013 %>%
  filter(STATEFP == "29") %>%
  st_transform("+init=epsg:4326")

colfunc <- colorRampPalette(c("#bfd3e6", "#6e016b"))

bins <- c(0, 3, 5, 7, 9)
pal <- colorBin(colfunc(4), domain = mo_flips_longlat$nodeFlips, bins = bins)
mo_flips_longlat <- st_transform(mo_flips_sf, "+init=epsg:4326")

labels <- ifelse(mo_flips_longlat$nodeFlips == 1, 
                 sprintf(
                   "<strong>%s</strong><br/>Congressional district %s<br/>%g flip",
                   mo_flips_longlat$NAMELSAD10, mo_flips_longlat$CD, mo_flips_longlat$nodeFlips
                 ) %>% lapply(htmltools::HTML),
                 sprintf(
                   "<strong>%s</strong><br/>Congressional district %s<br/>%g flips",
                   mo_flips_longlat$NAMELSAD10, mo_flips_longlat$CD, mo_flips_longlat$nodeFlips
                 ) %>% lapply(htmltools::HTML))



leaflet(data = mo_flips_longlat) %>%
  addPolygons(
              fillColor = ~pal(nodeFlips),
              opacity = 1,
              color = 'white',
              weight = .5,
              fillOpacity = .5,
              smoothFactor = 0,
              highlightOptions = highlightOptions(
                color = "darkgray",
                weight = 2,
                bringToFront = FALSE
              ),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addSearchOSM(options = searchFeaturesOptions(hideMarkerOnCollapse = TRUE)) %>%
  addResetMapButton() %>%
addLegend("bottomleft", pal = pal, values = ~nodeFlips,
          title = "Number of Flips <br>in Markov Chain",
          opacity = 1,
          labFormat = function(type, cuts, p) {
            n = length(cuts)
            paste0(cuts[-n], " to ", cuts[-1] -1)
          }) %>%
  addMiniMap(width = 225, 
             height = 225,
             toggleDisplay = TRUE,
             zoomAnimation = TRUE) %>%
  addPolylines(data = cd_2013_mo,
             color = "#7a7a7a",
             weight = 2,
             group = "Show 2012 Congressional Districts") %>%
  addPolylines(data = state_2013_mo,
               color = "#7a7a7a",
               weight = 2) %>%
  addLayersControl(overlayGroups = c("Show 2012 Congressional Districts"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "topright")
