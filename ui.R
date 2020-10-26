library(shiny)
library(leaflet) # for the map
library(shinythemes)
library(dplyr) # filter and reactive
library(geosphere) # for the distance between two points
library(shinydashboard) # for the display

library(readr)
ships <- read_csv(file= "ships.csv", n_max=10000)
attach(ships)


######################################################

dashboardPage(
  dashboardHeader(title = "Vessel app"),
  dashboardSidebar(selectizeInput('typ', 'Select vessel type', 
                                  choices = ships$ship_type,
                                  selected=TRUE),
                   selectizeInput('nam', 'Select vessel name', 
                                  choices = ships$SHIPNAME,
                                  selected=TRUE),
                   box(background = "light-blue", width= 8,
                       paste("Selected vessel's route shown on map"))),
  
  dashboardBody(box(width=12,
                    leafletOutput("mymap"),
                    box(color="light-blue", width=10, textOutput("maxdist") )
  )
  ))
