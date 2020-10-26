library(shiny)
library(leaflet) # for the map
library(shinythemes)
library(dplyr) # filter and reactive
library(geosphere) # for the distance between two points
library(shinydashboard) # for the display


library(readr)
ships <- read_csv(file= "ships.csv", n_max=10000)
attach(ships)


##############################################################

function(input, output, session) {
  
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng=18.6676,lat=54.6,zoom=9) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircles(lng=18.6676,lat=54.3894, opacity=0.01)
    
  })
  
  
  
  nam.choice <- reactive({
    ships %>% 
      filter(ship_type == input$typ) %>%
      pull(SHIPNAME)
  })
  
  
  observe({
    
    updateSelectizeInput(session, "nam", choices = nam.choice())
    
  })
  
  
  output$maxdist<- renderPrint({
    
    # The loop: extracts data of the vessel selected from original data.frame
    # Then calculates distance between two consecutive points
    # Calculation is done after to avoid calculating distance between different ships
    
    ####################
    
    filtdata<- ships %>%
      filter(SHIPNAME == input$nam)
    attach(filtdata)
    a=1
    d=1
    while(a<(nrow(filtdata))){
      d[a]<- c(distm(c(LON[a], LAT[a]), c(LON[a+1], LAT[a+1]), 
                     fun = distHaversine))
      a=a+1
    }
    d[nrow(filtdata)]<- 0.1
    filtdata$dist<- d
    ################
    
    result<- max(filtdata$dist, na.rm=TRUE)
    paste("Max distance between two consecutive observations (green and red) is", 
          format(round(result, 3), nsmall = 3), "meters")
  })
  
  observeEvent(input$nam, {
    
    if(input$nam != "")
    {
      leafletProxy("mymap") %>% clearGroup(group="one")
      index = which(ships$SHIPNAME == input$nam)
      leafletProxy("mymap")%>% 
        addCircleMarkers(lng = ships$LON[index],lat = ships$LAT[index],
                         group="one", fill = FALSE,
                         weight = 2, radius=2, opacity=0.05,color= "blue")
      
      ####################
      filtdata<- ships %>%
        filter(SHIPNAME == input$nam)
      attach(filtdata)
      a=1
      d=1
      while(a<(nrow(filtdata))){
        d[a]<- c(distm(c(LON[a], LAT[a]), c(LON[a+1], LAT[a+1]), 
                       fun = distHaversine))
        a=a+1
      }
      d[nrow(filtdata)]<- 0.1
      filtdata$dist<- d
      ################
      index2 = which.max(filtdata$dist)
      leafletProxy("mymap")%>% 
        addCircleMarkers(lng = filtdata$LON[index2], lat = filtdata$LAT[index2],
                         group="one", fill=TRUE, weight = 2, radius=4,
                         color= "black", fillColor ="green", label="start",
                         layerId="circles1")
      
      index3 = index2 +1
      leafletProxy("mymap")%>% 
        addCircleMarkers(lng = filtdata$LON[index3], lat = filtdata$LAT[index3],
                         group="one", fill=TRUE, weight = 2, radius=4,
                         color= "black", fillColor ="red", label="end",
                         layerId="circles2")
      
      
      
    }
  }) 
}
