
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
            summarise("registered_patients" = sum(registered_patients),
                      "items" = sum(items), 
                      "quantity" = sum(quantity), 
                      "actual_cost" = sum(actual_cost)) %>% 
            ungroup() %>% 
            mutate("items_per_1000" = (items/(registered_patients/1000)),
                   "quantity_per_1000" = (quantity/(registered_patients/1000)),
                   "actual_cost_per_1000" = (actual_cost/(registered_patients/1000))) %>% 
            select(-registered_patients)

            })

    df_for_map <- reactive({
        shp_files %>% inner_join(df_for_bar())
        })


# Render the data table ---------------------------------------------------

    output$data_table <- DT::renderDataTable(df,
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
                    lng = 0,
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
    

# Create the line chart ---------------------------------------------------

    output$line_chart <- renderPlotly({

        df_for_line() %>%
            group_by(date, chemical, bnf_code) %>% 
            summarise(mean_variable = mean(get(input$variable), na.rm = TRUE),
                      sd_variable = sd(get(input$variable), na.rm = TRUE),
                      n_variable = n()) %>% 
            ungroup() %>% 
            mutate(se_variable = sd_variable/sqrt(n_variable),
                   lower_ci = mean_variable - qt(1 - (0.05/2), n_variable - 1) * se_variable,
                   upper_ci = mean_variable + qt(1 - (0.05/2), n_variable - 1) * se_variable) %>%
            plot_ly(x = ~date,
                    y = ~round(mean_variable, 2),
                    type = "scatter",
                    mode = "lines+markers",
                    line = list(color = "#004650"),
                    name = 'National Average',
                    marker = list(color = "#004650",
                                size = 4)) %>%
            add_ribbons(ymin = ~round(lower_ci, 2),
                        ymax = ~round(upper_ci, 2),
                        fillcolor = 'rgba(0,70,80,0.2)',
                        line = list(color = 'transparent'),
                        marker = list(color = "transparent"),
                        name = '95% Confidence interval',
                        hoverinfo = "none"
                        ) %>% 
            layout(#title = list(text = "<b>Prescribing over 5 years in England</b>",
                   #            x = 0.1),
                   yaxis = list(title = "<b>Example Y-axis Title</b>",
                                tickformat = ",",
                                rangemode = "tozero"),
                   xaxis = list(title = FALSE,
                                type = 'date',
                                tickformat = "%b<br>%Y"),
                   hovermode = "x unified",
                   legend = list(x = 0.01,
                                 y = 0.99))
        
    })
    
    

# Create the bar chart ----------------------------------------------------

    
    output$bar_chart <- renderPlotly({
        
        df_for_bar() %>%
            plot_ly(x = ~reorder(ods_code, get(input$variable)),
                    y = ~get(input$variable),
                    type = "bar",
                    hoverinfo = "text",
                    hovertext = paste0("Name: ", df_for_bar()$name,
                                       "<br>ODS code: ", df_for_bar()$ods_code,
                                       "<br>GSS code: ", df_for_bar()$gss_code,
                                       "<br>Items: ", round(df_for_bar()[[input$variable]], 2)),
                    showlegend = FALSE,
                    color = I("#004650"),
                    alpha = 0.6) %>%
            add_lines(y = ~median(get(input$variable), na.rm = TRUE),
                      name = "National Median",
                      line = list(dash = 'dot',
                                  width = 2,
                                  color = "#393939"),
                      hovertext = paste0("National Median: ", median(df_for_bar()[[input$variable]]))) %>%
            add_annotations(xref = "paper",
                            yref ="y",
                            x = 0.05,
                            y = median(median(df_for_bar()[[input$variable]]), na.rm = TRUE)*1.3,
                            text = "<b>National median</b>",
                            showarrow = FALSE,
                            font = list(color = '#393939',
                                        size = 12)) %>% 
            layout(yaxis = list(title = "<b>Example Y-axis Title</b>",
                                tickformat = ",",
                                rangemode = "tozero"),
                   xaxis = list(title = FALSE,
                                showticklabels = FALSE),
                   hovermode = "x unified")
 
    })
    

    
    # Note: if we want to use caching, RDT server must be set to False
}

# temp <- df %>% filter(chemical == "Apixaban",
#                area_type == "ccg") %>%
#     group_by(date, chemical, bnf_code, name, ods_code, gss_code) %>%
#     summarise("registered_patients" = sum(registered_patients),
#               "items" = sum(items),
#               "quantity" = sum(quantity),
#               "actual_cost" = sum(actual_cost)) %>%
#     ungroup() %>%
#     mutate("items_per_1000" = (items/(registered_patients/1000)),
#            "quantity_per_1000" = (quantity/(registered_patients/1000)),
#            "actual_cost_per_1000" = (actual_cost/(registered_patients/1000)))
# 
# 
# bar_temp <- temp %>%
#     filter(date >= "2020-01-01",
#            date <= "2021-01-01") %>%
#     group_by(chemical, bnf_code, name, ods_code, gss_code) %>%
#     summarise("registered_patients" = sum(registered_patients),
#               "items" = sum(items),
#               "quantity" = sum(quantity),
#               "actual_cost" = sum(actual_cost)) %>%
#     ungroup() %>%
#     mutate("items_per_1000" = (items/(registered_patients/1000)),
#            "quantity_per_1000" = (quantity/(registered_patients/1000)),
#            "actual_cost_per_1000" = (actual_cost/(registered_patients/1000))) %>%
#     select(-registered_patients)
# 
# 
# bar_temp %>%
#     #mutate(median_centred = items - median(items)) %>%
#     plot_ly(x = ~reorder(ods_code, items),
#             y = ~items,
#             type = "bar",
#             hoverinfo = "text",
#             hovertext = paste0("Name: ", bar_chart_df$name,
#                                "<br>ODS code: ", bar_chart_df$ods_code,
#                                "<br>GSS code: ", bar_chart_df$gss_code,
#                                "<br>Items: ", bar_chart_df$items)) %>%
#     layout(yaxis = list(title = "<b>Example Y-axis Title</b>",
#                         tickformat = ",",
#                         rangemode = "tozero"),
#            xaxis = list(title = FALSE,
#                         showticklabels = FALSE),
#            hovermode = "x unified",
#            shapes=list(type='line',
#                        xref = "paper",
#                        x0= 0,
#                        x1= 1,
#                        yref = "y",
#                        y0 = ~median(items, na.rm = TRUE),
#                        y1 = ~median(items, na.rm = TRUE),
#                        line=list(dash='dot',
#                                  width=2),
#                        name = "test"),
#            legend = list(x = 0.01,
#                          y = 0.99))
# 
# bar_temp %>%
#     #mutate(median_centred = items - median(items)) %>%
#     plot_ly(x = ~reorder(ods_code, items),
#             y = ~items,
#             type = "bar",
#             hoverinfo = "text",
#             hovertext = paste0("Name: ", bar_temp$name,
#                                "<br>ODS code: ", bar_temp$ods_code,
#                                "<br>GSS code: ", bar_temp$gss_code,
#                                "<br>Items: ", bar_temp$items),
#             showlegend = FALSE,
#             color = I("#004650"),
#             alpha = 0.6) %>%
#     add_lines(y = ~median(items, na.rm = TRUE),
#               name = "National Median",
#               line = list(dash = 'dot',
#                           width = 2,
#                           color = "#393939"),
#               hovertext = paste0("National Median: ", median(bar_temp$items, na.rm = TRUE))) %>%
#     add_annotations(xref = "paper",
#                     yref ="y",
#                     x = 0.05,
#                     y = median(bar_temp$items, na.rm = TRUE)*1.15,
#                     text = "<b>National median</b>",
#                     showarrow = FALSE) %>% 
#     layout(yaxis = list(title = "<b>Example Y-axis Title</b>",
#                         tickformat = ",",
#                         rangemode = "tozero"),
#            xaxis = list(title = FALSE,
#                         showticklabels = FALSE),
#            hovermode = "x unified")
