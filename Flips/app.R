pacman::p_load(shiny, ggplot2, dplyr, sf, leaflet.extras, leaflet, shinythemes, ggthemes, extrafont)

# load data from data/

mo_flips_longlat <- readRDS("data/mo_flips_longlat.RDS")
cd_2013_mo <- readRDS("data/cd_2013_mo.RDS")
state_2013_mo <- readRDS("data/state_2013_mo.RDS")

# setting up color palettes for leaflet map

colfunc <- colorRampPalette(c("#bfd3e6", "#6e016b"))

bins <- c(0, 1, 4, 7, 9) # hard coded for now, fix later

pal <- colorBin(colfunc(4), domain = mo_flips_longlat$nodeFlips, bins = bins)

labels <- ifelse(mo_flips_longlat$nodeFlips == 1, 
                 sprintf(
                   "<strong>%s</strong><br/>Congressional district %s<br/>%g flip",
                   mo_flips_longlat$NAMELSAD10, mo_flips_longlat$CD, mo_flips_longlat$nodeFlips
                 ) %>% lapply(htmltools::HTML),
                 sprintf(
                   "<strong>%s</strong><br/>Congressional district %s<br/>%g flips",
                   mo_flips_longlat$NAMELSAD10, mo_flips_longlat$CD, mo_flips_longlat$nodeFlips
                 ) %>% lapply(htmltools::HTML))

ui <- shinyUI(fluidPage(
  theme = shinytheme("cosmo"),
  tags$head(
    tags$style(HTML(".leaflet-container { background: #F2EFE9; }")) # set the background color
  ),
  titlePanel("Markov Chain Flips: Missouri VTDs"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
      helpText("Select the number of bins to display on the histogram below."),
      numericInput("num", 
                   h3("Bins:"), 
                   value = 10),
      plotOutput("histogram")),
  
  mainPanel(
            fluidRow(
              splitLayout(leafletOutput("mymap", height = 800))
    
    )
  )
)
)
)
server <- function(input, output, session) {
  
  output$histogram <- renderPlot({
    ggplot(mo_flips_longlat, aes(x = nodeFlips)) +
      geom_histogram(bins = input$num, fill = "#A48DBD", color = "white") +
      theme_hc() +
      labs(y = "Number of VTDs",
           x = "Number of district flips") +
      theme(text = element_text(family = "Century Gothic"))
      
  })
  
  output$mymap <- renderLeaflet({
     leaflet(data = mo_flips_longlat) %>%
      addPolygons(
        fillColor = ~pal(nodeFlips),
        opacity = 1,
        color = 'white',
        weight = .5,
        fillOpacity = 0.75,
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
      addLegend("bottomleft", 
                pal = pal, 
                values = ~nodeFlips,
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
    
  })

}

shinyApp(ui, server, options = list(height = 1080))

