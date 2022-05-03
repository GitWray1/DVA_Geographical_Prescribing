
# Load packages -----------------------------------------------------------

library(dplyr)
library(leaflet)
library(plotly)




# Line chart - using plotly -----------------------------------------------

line_df <- df %>% filter(chemical == "Apixaban",
                      area_type == "region") %>% 
                  group_by(date, chemical, bnf_code, name, ods_code, gss_code) %>% 
                  summarise("registered_patients" = sum(registered_patients),
                            "items" = sum(items), 
                            "quantity" = sum(quantity), 
                            "actual_cost" = sum(actual_cost)) %>% 
                  ungroup() %>% 
                  mutate("items_per_1000" = (items/(registered_patients/1000)),
                         "quantity_per_1000" = (quantity/(registered_patients/1000)),
                         "actual_cost_per_1000" = (actual_cost/(registered_patients/1000)))


line_df %>%
    plot_ly(x = ~date, 
            y = ~items_per_1000,
            type = "scatter", 
            mode = "lines+markers",
            color = ~chemical,
            symbol = ~chemical) %>% 
    layout(title = list(text = "<b>Example chart title</b>",
                        x = 0.1),
           yaxis = list(title = "<b>Example Y-axis Title</b>",
                        tickformat = ",",
                        rangemode = "tozero"),
           xaxis = list(title = FALSE,
                        type = 'date',
                        tickformat = "%b<br>%Y"),
           hovermode = "x unified",
           legend = list(title = list(text = "Medicine"), 
                         x = 100, 
                         y = 0.5))


# Bar chart ---------------------------------------------------------------

bar_chart_df %>% 
    plot_ly(x = ~reorder(ods_code, items),
            y = ~items,
            type = "bar",
            hoverinfo = "text",
            hovertext = paste0("Name: ", bar_chart_df$name,
                               "<br>ODS code: ", bar_chart_df$ods_code,
                               "<br>GSS code: ", bar_chart_df$gss_code,
                               "<br>Items: ", bar_chart_df$items)) %>% 
    layout(title = list(text = "<b>Example chart title</b>",
                        x = "0.1"),
           yaxis = list(title = "<b>Example Y-axis Title</b>",
                        tickformat = ",",
                        rangemode = "tozero"),
           xaxis = list(title = FALSE,
                        showticklabels = FALSE),
           hovermode = "x unified")


# Create leaflet map ------------------------------------------------------

# Create leaflet map - need to add legend and sort out labels 
data_shp_df %>% 
    leaflet(options = leafletOptions(zoomDelta = 0.5,
                                     zoomSnap = 0.5)) %>% 
    setView(lat = 53, 
            lng = -1.5, 
            zoom = 6.5) %>%
    addProviderTiles(provider = "CartoDB.Positron") %>% 
    addPolygons(fillColor = ~pal(items_per_1000),
                 fillOpacity = 0.3,
                 color = "#393939",
                 weight = 1.5,
                 opacity = 1,
                 highlight = highlightOptions(weight = 3,
                                              color = "#222222",
                                              fillOpacity = 0.7,
                                              bringToFront = TRUE),
                label = ~lapply(paste0("<strong>Name: </strong>", data_shp_df$name,
                                       "<br><strong>Items per 1000:</strong> ", round(data_shp_df$items_per_1000, 2)), 
                                htmltools::HTML),
                labelOptions = labelOptions(textsize = "12px",
                                            style = list("font-family" = "Arial")))
