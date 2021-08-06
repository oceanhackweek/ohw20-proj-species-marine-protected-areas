library(shiny)
library(shinythemes)
library(leaflet)
# Define UI for application that draws a histogram

countries <- c("USA", "Uruguay", "Spain", "Portugal", "Ireland", "Iceland", "Cuba",
               "Belgium", "Chile", "Argentina")
names(countries) <- countries

ui <- navbarPage(title = "", 
                 theme = shinytheme("flatly"),
                 
                 ## THIS IS WHAT WE HAD BEFORE I STARTED MESSING AROUND WITH DESIGN.
                  ## leaving it at least until tomorrow morning in case we want to 
                  ## revert back to what we had:
                 
                 #tabPanel("Map",
                 #          fluidRow(
                 #              column(8, leafletOutput("ui_mymap")),
                 #              column(4, "Highlighted MPAs",
                 #                     fluidRow(uiOutput("mpa_highlight")),
                 #                     # need to stop the first column from being cut off
                 #                     div(style="width:400px;overflow-x: scroll;height:400px;overflow-y: scroll;",
                 #                         fluidRow(tableOutput("mpa_highlight_table")))
                 #              )
                 #              )
                 #          ),
                 #          fluidRow(
                 #              column(4, 
                 #                     fluidRow(selectInput(inputId = "country_selection", label ="Country selection:", choices=countries)),
                 #                     fluidRow(uiOutput("ui_filter_mpas"))
                 #                     ),
                 #              column(2, 
                 #                     fluidRow(actionButton(inputId="update_country", "Update country")),
                 #                     fluidRow(uiOutput("ui_update_filter"))
                 #                     ),
                 #              column(6,
                 #                     plotOutput(outputId = "obis_plot"))
                 
                 ## THIS IS THE NEW DESIGN:
                 
                 tabPanel("Explore the Map", 
                          # add the side bar
                          sidebarLayout(
                              sidebarPanel(
                                  fluidRow(selectInput(inputId = "country_selection", label ="Select a country:", choices=countries)),
                                  fluidRow(actionButton(inputId="update_country", "Update country")), 
                                  br(),
                                  fluidRow(uiOutput("ui_filter_mpas")),
                                  fluidRow(uiOutput("ui_update_filter")),
                                  br(),
                                  fluidRow(uiOutput("mpa_highlight"))
                                          ),
                              mainPanel(leafletOutput("ui_mymap"))
                              ),
                          fluidRow(
                            uiOutput("about_this_mpa")
                            ),
                          br(),
                          fluidRow(
                            column(6,
                                   fluidRow(tableOutput("mpa_highlight_table"))#)
                                   ),
                            column(6,
                                   plotOutput(outputId = "obis_plot"))
                            ),
                          ),
                 # second tab
                 tabPanel("About", "A bit of background, methods, how to use the app") #,
                 
                # Commenting this out for now, but leaving in case we want it back in
                 # tabPanel("OBIS options",
                 #          sidebarLayout(
                 #              sidebarPanel(
                 #                  sliderInput("bins",
                 #                              "Number of bins:",
                 #                              min = 1,
                 #                              max = 50,
                 #                              value = 30)
                 #              ),
                 #              
                 #              # Show a plot of the generated distribution
                 #              mainPanel(
                 #                  tabsetPanel(
                 #                      tabPanel("Tab label1", plotOutput("distPlot")),
                 #                      tabPanel("tab label2", "content")
                 #                  )
                 #              )
                 #          ))
                 
)
