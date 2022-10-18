#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)

library(dplyr)

library(leaflet)

library(DT)

library(sf)


##define server logic
##the "zoom" column code was entirely taken from https://urbandatapalette.com/post/2022-05-shiny-zoom-selected-features/
shinyServer <- function(input, output, session) {
  
  ###import data
  restaurants <- read.csv("asian_restaurants_yelp_flag_2022-10-13.csv")
  restaurants <- data.frame(restaurants) %>%
    mutate(zoom =
             paste('<a class="go-map" href=""',
                   'data-lat="', latitude, '" data-lng="', longitude,
                   '"><i class=\"fa fa-search\"></i></a>',
                   sep="")
    ) %>%
    arrange(full_country_name)%>%
    select(X, full_country_name, id, name, address1, city, state, zoom,rating, review_count,latitude, longitude, categories_1, categories_2, ccode, flag_url)
  restaurants$latitude <-  as.numeric(restaurants$latitude)
  restaurants$longitude <-  as.numeric(restaurants$longitude)
  groups <- as.character(unique(restaurants$full_country_name))
  urls <- as.character((unique(restaurants$flag_url)))
  var_list <- list(group_list=groups, url_list=urls)
  
  ###popup settings
  restaurants <- mutate(restaurants, cntnt=paste0('<strong>Name: </strong>',name,
                                                  '<br><strong>Rating:</strong> ', rating,
                                                  '<br><strong>Number of Reviews:</strong> ', review_count,
                                                  '<br><strong>Address:</strong> ',address1,
                                                  '<br><strong>City:</strong> ',city))
  
  # create the leaflet map  
  output$restaurants <- renderLeaflet({
    themap <- leaflet(restaurants) %>% 
      ###basemap
      addProviderTiles(providers$CartoDB.Positron)  
  
    
      ###markers
      for (i in 1:length(var_list$group_list)){
        themap <- addMarkers(map=themap,data = subset(restaurants, restaurants$full_country_name==var_list$group_list[i])
                   , lat =  ~latitude, lng =~longitude, 
                   icon = makeIcon(urls[i], iconWidth = 10, iconHeight = 8)
                   , group = var_list$group_list[i]
                   ,popup = ~as.character(cntnt))
      }
  themap <- themap %>% 
    ##dropdown list
    addLayersControl(position = "topleft",
                     overlayGroups =var_list$group_list,
                     options = layersControlOptions(collapsed = TRUE)) %>%
    ##current location button
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="Current Location",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
     ###title of dropdown
     htmlwidgets::onRender("
      function() {
          $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Country</label>');
      }
       ")
  
  themap    
  })
  ## add zoom button https://urbandatapalette.com/post/2022-05-shiny-zoom-selected-features/
  observe({
      # escape initialise state
      if (is.null(input$goto)) return()
      
      isolate({
        map = leafletProxy("restaurants")
        
        lat = input$goto$lat
        lng = input$goto$lng
        
        setView(map, lng, lat, zoom = 17)
      })
    })
  #create a data object to display data in table below map
  output$data <-DT::renderDataTable(server = FALSE,{
    
    # `rownames` needs to be consistent with `DT::datatable` option
    action <- DT::dataTableAjax(session, restaurants, rownames = FALSE)
    datatable(
    restaurants[,c(2,4,5,6,7,8,9,10)],filter = 'top',
    rownames = FALSE,
    options = list(ajax = list(url = action)),
    # Render HTML tags inside table (e.g. fontawesome icons in <i> tags)
    escape = FALSE
  )})
 
  
}

