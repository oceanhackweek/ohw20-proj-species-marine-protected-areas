library(shiny)
library(leaflet)
# Define UI for application that draws a histogram
ui <- navbarPage(title = "Nav: ",
                 tabPanel("Map",
                          fluidRow(
                              leafletOutput("mymap")
                          ),
                          fluidRow(
                              column(4, selectInput("country_selection", "Country selection:", c("Spain"="Spain", "Cuba"="Cuba", "Chile"="Chile"))),
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
