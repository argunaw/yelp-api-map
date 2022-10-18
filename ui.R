library(shiny)
library(leaflet)
library(shinydashboard)

navbarPage("Asian Restaurants in NYC", id="main",
           tabPanel("Map",  leafletOutput("restaurants", height=650),
                    includeScript("gomap.js"),
                    DT::dataTableOutput("data")),
            tabPanel("Source",includeMarkdown("README_app.md")))


