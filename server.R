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
        mpas <- mpa_match(country(), st_point(c(click$lng, click$lat)))[[1]] # get row numbers of MPAs selected
        mpas <- country()[mpas,] # extract MPAs
        leafletProxy("mymap") %>%
            addPopups(data=mpas, lng=click$lng, lat=click$lat, popup=~sprintf("%s records found", nrow(mpas)))
        # next step is to instead of having the info on the popup, have the info for each clicked mpa on a sidebar to the right
        output$mpa_highlight <- renderUI(
            selectInput(inputId = "mpa_select", label="MPA:", choices=mpas$WDPAID)
        )
        # need to make this vertical
        output$mpa_highlight_table <- renderTable({
            req(input$mpa_select)
            df<-filter(mpas, WDPAID==input$mpa_select) %>% as_tibble() %>% select(-geom) %>% t() %>% as.data.frame() %>% tibble::rownames_to_column()
            print(dim(df))
            df
        },
        colnames=FALSE, spacing = "xs", width="100%", align="l")
    })
    


    
}
