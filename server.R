## Notes: Put as little as possible within the server function to reduce server load???


## Libraries
# should replace with an all purpose library installer and loader
library(shiny)
library(sf)
library(dplyr)

## Server functions ##
source("R-code/wdpar-package.R")

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Tab 2
    output$distPlot <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
    })
    
    #MPA map
    # for some reason only Cuba works
    country <- reactive({
        wdpa_read_country(input$country_selection)
    }) %>% bindEvent(input$update_country)
    
    # trying to fine a way to cache the country wdpas 
    # %>% bindCache(input$country_selection) %>% bindEvent(input$update_country)
    
    
    output$mymap <- renderLeaflet({
        # render the base leaflet map
        leafletplot <- leaflet() %>% addProviderTiles(providers$Stamen.Watercolor, options = providerTileOptions(noWrap = TRUE))
        if(is(country())[1]=="sf")# what to put here to only run the following code if a WDPA has been loaded into country? and else render a regular map?
            print("this code was run")
            leafletplot <- leafletplot %>% addPolygons(data=country())
    })
    observeEvent(input$mymap_shape_click, {
        click <- input$mymap_shape_click
        mpas <- country()[mpa_match(country(), st_point(c(click$lng, click$lat)))[[1]],]
        leafletProxy("mymap") %>% 
            addPopups(data=match, lng=click$lng, lat=click$lat, popup=~sprintf("%s", NAME)) #kind of works but popups are overlapping, unfortunately popups require input that simplifies into a string, and I'm not interested in wrting long HTML code
        # next step is to instead of having the info on the popup, have the info for each clicked mpa on a sidebar to the right
    })

    
}
