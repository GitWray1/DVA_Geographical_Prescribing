

# Filter for map ----------------------------------------------------------

filter_for_map <- function(bar_df, shape_files){
    temp <- shp_files %>% inner_join(bar_df)
    return(temp)
}

# Create the base map -----------------------------------------------------

create_start_map <- function(){
    
    temp <- leaflet(options = leafletOptions(zoomDelta = 0.25,
                                             zoomSnap = 0.25)) %>% 
        setView(lat = 53,
                lng = 0,
                zoom = 6.75) %>%
        addProviderTiles(provider = "CartoDB.Positron")
    return(temp)
}


# Update the map with user input ------------------------------------------

update_map <- function(map_df, input_variable, pal, rv){
    
    pal <- colorBin("plasma",
                    domain = map_df[[input_variable]],
                    bins = 5,
                    na.color = "#808080")
    #pal <- colorNumeric("plasma", domain = df_for_map()[[input$variable]])
    
    temp <- leafletProxy("mymap", data = map_df) %>%
        clearShapes() %>%
        addPolygons(fillColor = ~pal(map_df[[input_variable]]),
                    fillOpacity = 0.3,
                    color = "#393939",
                    weight = 1.5,
                    opacity = 1,
                    layerId = ~ods_code,
                    highlight = highlightOptions(weight = 3,
                                                 color = "#222222",
                                                 fillOpacity = 0.7,
                                                 bringToFront = TRUE),
                    label = ~lapply(paste0("<strong>Name: </strong>", map_df$name,
                                           "<br><strong>", get_y_title(input_variable),":</strong> ", tidy_number(map_df[[input_variable]])),
                                    htmltools::HTML),
                    labelOptions = labelOptions(textsize = "12px",
                                                style = list("font-family" = "Arial"))) %>%
        clearControls() %>%
        addLegend(position = "bottomleft",
                  pal = pal,
                  values = ~map_df[[input_variable]],
                  title = get_y_title(input_variable),
                  opacity = 0.7)
    return(temp)
}

