## Notes: Put as little as possible here to reduce server load

library(shiny)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Tab 1
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })
    
    #Tab 2
    points <- eventReactive(input$recalc, {
        cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL=FALSE)
    output$mymap <- renderLeaflet({
        leaflet() %>% addProviderTiles(providers$Stamen.Watercolor, options = providerTileOptions(noWrap = TRUE)) %>% 
            addMarkers(data=points())
    })

}
