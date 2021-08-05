library(shiny)
library(leaflet)
# Define UI for application that draws a histogram

countries <- c("USA", "Uruguay", "Spain", "Portugal", "Ireland", "Iceland", "Cuba", "Belgium", "Chile", "Argentina")
names(countries) <- countries

ui <- navbarPage(title = "Nav: ",
                 tabPanel("Map",
                          fluidRow(
                              column(8, leafletOutput("mymap")),
                              column(4, "Highlighted MPAs",
                                     fluidRow(uiOutput("mpa_highlight")),
                                     # need to stop the first column from being cut off
                                     div(style="width:400px;overflow-x: scroll;height:400px;overflow-y: scroll;",
                                         fluidRow(tableOutput("mpa_highlight_table")))
                              )
                          ),
                          fluidRow(
                              column(4, selectInput(inputId = "country_selection", label ="Country selection:", choices=countries)),
                              column(2, actionButton("update_country", "Update"))
                          ),
                 ),
                 tabPanel("OBIS options",
                          sidebarLayout(
                              sidebarPanel(
                                  sliderInput("bins",
                                              "Number of bins:",
                                              min = 1,
                                              max = 50,
                                              value = 30)
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                  tabsetPanel(
                                      tabPanel("Tab label1", plotOutput("distPlot")),
                                      tabPanel("tab label2", "content")
                                  )
                              )
                          )),
                 tabPanel("Tab3", "more contents here")
                 
)
