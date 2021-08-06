## Notes: Put as little as possible within the server function to reduce server load???


## Libraries
# should replace with an all purpose library installer and loader
library(shiny)
library(sf)
library(dplyr)
library(ggplot2)
library(robis)

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
    
    # MPA map
    # for some reason only coms countries work
    country <- reactive({
        wdpa_read_country(input$country_selection)
    }) %>% bindEvent(input$update_country)
    
    # country <- reactive({
    #     req(input$update_country)
    #     country() %>% filter(STATUS_YR==input$status_yr)
    # }) %>% bindEvent(input$update_filter)
    
    # trying to fine a way to cache the country wdpas 
    # %>% bindCache(input$country_selection) %>% bindEvent(input$update_country)

    output$ui_filter_mpas <- renderUI({
        req(input$update_country)
        yr.min <- min(country()$STATUS_YR)
        yr.max <- max(country()$STATUS_YR)
        tagList(

            sliderInput(inputId = "status_yr_range", label="Filter by Year Established:", min = yr.min, max=yr.max, step=1, value=c(yr.min,yr.max), sep = ""),
            checkboxGroupInput(inputId="iucn_cat", label="Filter by IUCN Categories:", choices=unique(country()$IUCN_CAT[order(match(country()$IUCN_CAT, c("Ia","Ib","II","III","IV","V","VI","Not Applicable","Not Assigned","Not Reported")))]))

        )
    })
    
    
    output$ui_update_filter <- renderUI({
        req(input$update_country)
        actionButton(inputId = "update_filter", label="Update filter")
    })
    
    
    output$ui_mymap <- renderLeaflet({
        # render the base leaflet map
        leafletplot <- leaflet() %>% addProviderTiles(providers$Stamen.Watercolor, options = providerTileOptions(noWrap = TRUE))
        if(is(country())[1]=="sf")# what to put here to only run the following code if a WDPA has been loaded into country? and else render a regular map?
            leafletplot <- leafletplot %>% addPolygons(data=country())

    })
    
    # Subset country data based on year est. and IUCN category selection
    iucn <- reactive({
        country() %>% 
            filter(., IUCN_CAT %in% input$iucn_cat & STATUS_YR >= min(input$status_yr_range) & STATUS_YR <= max(input$status_yr_range))
    }) %>% bindEvent(input$update_filter)
    
    # Incremental changes to the map: re-render map based on iucn selection
    observe({
        leafletProxy("ui_mymap", data = iucn()) %>%
            clearShapes() %>% 
            addPolygons()
            
    })
    
    
    observeEvent(input$ui_mymap_shape_click, {
        click <- input$ui_mymap_shape_click
        print(click)
        mpas <- mpa_match(country(), st_point(c(click$lng, click$lat)))[[1]] # get row numbers of MPAs selected
        mpas <- country()[mpas,] # extract MPAs matching click
        leafletProxy("ui_mymap") %>%
            addPopups(data=mpas, lng=click$lng, lat=click$lat, popup=~sprintf("%s records found", nrow(mpas)))
        output$mpa_highlight <- renderUI(
            selectInput(inputId = "mpa_select", label="If multiple MPAs overlap, select one:", choices=mpas$WDPAID)
        )
        # Render table with meta data about the selected MPA
        output$mpa_highlight_table <- renderTable({
            req(input$mpa_select)
            df<-filter(mpas, WDPAID==input$mpa_select) %>% as_tibble() %>% select(-geom) %>% t() %>% as.data.frame() %>% tibble::rownames_to_column()
        },
        colnames=FALSE, spacing = "xs", width="100%", align="l")
        
        # Fetch OBIS data for selected MPA and render plot showing placeholder plot for richness
        output$obis_plot <- renderPlot({
            req(input$mpa_select)
            df <- filter(mpas, WDPAID==input$mpa_select)
            obis_table <- robis::occurrence(geometry = sf::st_as_text(sf::st_convex_hull(sf::st_geometry(df)))) %>% 
                group_by(year) %>% 
                summarise(richness = n_distinct(genus)) %>% 
                filter(!is.na(year))
            ggplot(obis_table, aes(x=year, y=richness)) +
                geom_point(size=5, color="blue") +
                labs(x="", y="Distinct genera observed") + # axis labels need to be bigger
                theme_classic()
        })
        
        # Render text to appear with results (i.e. the plot and table)
        output$about_this_mpa <- renderUI({
            req(input$mpa_select)
            h3("About this MPA:")
        })
    })
    


    
}
