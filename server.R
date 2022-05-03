
# Load packages -----------------------------------------------------------

library(shiny)
library(dplyr)


# Create server  ----------------------------------------------------------

server <- function(input, output, session) {
    

# Filter the data according to user input ---------------------------------
    
    df_for_line <- reactive({
        
        df %>% filter(chemical == input$medicine,
                      area_type == input$area) %>% 
            group_by(date, chemical, bnf_code, name, ods_code, gss_code) %>% 
            summarise("registered_patients" = sum(registered_patients), 
                      "items" = sum(items), 
                      "quantity" = sum(quantity), 
                      "actual_cost" = sum(actual_cost)) %>% 
            ungroup() %>% 
            mutate("items_per_1000" = (items/(registered_patients/1000)),
                   "quantity_per_1000" = (quantity/(registered_patients/1000)),
                   "actual_cost_per_1000" = (actual_cost/(registered_patients/1000)))
        })
    
    df_for_bar <- reactive({
        
        df_for_line() %>% 
            filter(date >= input$date_range[1],
                   date <= input$date_range[2]) %>% 
            group_by(chemical, bnf_code, name, ods_code, gss_code) %>% 
            summarise("items" = sum(items), 
                      "quantity" = sum(quantity), 
                      "actual_cost" = sum(actual_cost),
                      "items_per_1000" = mean(items_per_1000),  # mean isn't the way to do this
                      "quantity_per_1000" = mean(quantity_per_1000),
                      "actual_cost_per_1000" = mean(actual_cost_per_1000)) %>% 
            ungroup()
        
            })
    
    df_for_map <- reactive({
        shp_files %>% inner_join(df_for_bar()) ## might be easier to just change format of data table
        })


# Render the data table ---------------------------------------------------

    output$data_table <- DT::renderDataTable(df,
                                             filter = "top",
                                             options = list(autoWidth = TRUE,
                                                            searching = TRUE,
                                                            columnDefs = list(list(width = '400px',
                                                                                   targets = 5))))
    
    # Check data filter function
    output$test_table <- DT::renderDataTable(df_for_map2(),
                                             filter = "top",
                                             options = list(autoWidth = TRUE,
                                                            searching = TRUE,
                                                            columnDefs = list(list(width = '400px',
                                                                                   targets = 5))))

# Create the leaflet map --------------------------------------------------

    output$mymap <- renderLeaflet({
        leaflet(options = leafletOptions(zoomDelta = 0.25,
                                         zoomSnap = 0.25)) %>% 
            setView(lat = 53,
                    lng = -1.5,
                    zoom = 6.75) %>%
            addProviderTiles(provider = "CartoDB.Positron")
    })
    
    observeEvent(c(df_for_map(), input$variable), {
        leafletProxy("mymap", data = df_for_map()) %>% 
            clearShapes() %>% 
            addPolygons(fillColor = ~pal(df_for_map()[[input$variable]]),
                        fillOpacity = 0.3,
                        color = "#393939",
                        weight = 1.5,
                        opacity = 1,
                        highlight = highlightOptions(weight = 3,
                                                     color = "#222222",
                                                     fillOpacity = 0.7,
                                                     bringToFront = TRUE),
                        label = ~lapply(paste0("<strong>Name: </strong>", df_for_map()$name,
                                               "<br><strong>", input$variable,":</strong> ", round(df_for_map()[[input$variable]], 2)), 
                                        htmltools::HTML),
                        labelOptions = labelOptions(textsize = "12px",
                                                    style = list("font-family" = "Arial"))) %>% 
            clearControls() %>% 
            addLegend(position = "bottomleft",
                      pal = pal,
                      values = ~df_for_map()[[input$variable]],
                      title = input$variable,
                      opacity = 0.7)
    })
    
    
    
    # Note: if we want to use caching, RDT server must be set to False
}