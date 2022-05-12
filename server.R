
# Load packages -----------------------------------------------------------

library(shiny)
library(dplyr)

# Create server  ----------------------------------------------------------

server <- function(input, output, session) {
    
    output$testtext <- renderText(as.character(rv$click))
    
# Filter the data according to user input ---------------------------------
    
    df_for_line <- reactive({
        
        df %>% filter(chemical == input$medicine,
                      area_type == input$area) %>%
              # group_by(date, chemical, bnf_code, name, ods_code, gss_code) %>%
              # summarise("registered_patients" = sum(registered_patients),
              #           "items" = sum(items),
              #           "quantity" = sum(quantity),
              #           "actual_cost" = sum(actual_cost)) %>%
              # ungroup() %>%
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

# Create reactive values for charts ---------------------------------------

    # Set up reactive values to store variables
    rv <- reactiveValues(click = NULL)
    
    # Asign the clicked shape ID to the reactive variable rv$click
    observeEvent(input$mymap_shape_click, {
        rv$click <- input$mymap_shape_click
    })
    
    # Set up the button to return figures to national levels
    output$clear_click <- renderUI({
        if (!is.null(rv$click)) {
            actionButton("reset", "Remove area")
        }
    })

    # When the reset button is clicked return the rv$click value to NULL
    observeEvent(input$reset, {
        rv$click <- NULL
    })
    
    # Update Y axis title and store as reactive variable for use in plots
    observeEvent(input$variable, {
        rv$yaxis <- get_y_title(input$variable)
        })
    
    # Update Y axis title and store as reactive variable for use in plots
    observeEvent(input$area, {
        
        rv$area <- case_when(input$area == "ccg" ~ "CCG",
                             input$area == "stp" ~ "STP",
                             input$area == "region" ~ "Region",
                             TRUE ~ "Unknown area input")
    })
    
    # Update line chart title and store as reactive variable
    observeEvent(c(rv$yaxis, input$medicine), {
        rv$line_title <- get_line_title(rv$yaxis, input$medicine)
        })
    
    # Update line chart title and store as reactive variable
    observeEvent(c(rv$yaxis, input$medicine, input$area, input$date_range), {
        rv$bar_title <- get_bar_title(rv$yaxis, input$medicine, input$area, input$date_range)
        })


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
                        layerId = ~ods_code,
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
                      title = rv$yaxis,
                      opacity = 0.7)
    })
    

# Create the line chart ---------------------------------------------------
    
    output$line_chart <- renderPlotly({
        
       p <- df_for_line() %>%
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
                        hoverinfo = "none") %>% 
            layout(yaxis = list(title = rv$yaxis,
                                tickformat = ",",
                                rangemode = "tozero"),
                   xaxis = list(title = FALSE,
                                type = 'date',
                                tickformat = "%b<br>%Y"),
                   hovermode = "x unified",
                   legend = list(xanchor = "center",
                                 x = 0.5,
                                 y = 1.05,
                                 orientation = "h",
                                 bgcolor = "rgba(255,255,255,0.2)"),
                   title = list(text = stringr::str_wrap(paste0("<b>", rv$line_title,"</b>"), width = 80),
                                font = list(size = 12),
                                x = 0.05)) %>% 
            config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("zoom", "pan", "select", "lasso"))
        
       if (!is.null(rv$click)) {
           
           temp_df <- df_for_line() %>% 
                            filter(ods_code == rv$click[1])
           
           p %>% 
               add_trace(data = temp_df,
                         y = ~items,
                         type = "scatter",
                         mode = "lines+markers",
                         line = list(color = "#D5824D"),
                         connectgaps = TRUE,
                         name = ~name,
                         marker = list(color = "#D5824D",
                                       size = 4))
               
       } else {
           
           p
       }
       
    })
    

# Create the bar chart ----------------------------------------------------

    # Use plotly proxy?
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
                            y = median(df_for_bar()[[input$variable]], na.rm = TRUE),
                            yshift = 9,
                            text = "<b>National median</b>",
                            showarrow = FALSE,
                            font = list(color = '#393939',
                                        size = 12)) %>% 
            layout(yaxis = list(title = rv$yaxis,
                                tickformat = ",",
                                rangemode = "tozero"),
                   xaxis = list(title = list(text = paste0("<b>", rv$area,"s</b>"),
                                             font = list(size=12),
                                             standoff = 10),
                                showticklabels = FALSE),
                   hovermode = "x unified",
                   title = list(text = stringr::str_wrap(paste0("<b>", rv$bar_title,"</b>"), width = 80),
                                font = list(size = 12),
                                x = 0.05)) %>% 
            config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("zoom", "pan", "select", "lasso"))
 
    })
    
    output$infotext <- renderText({
        create_text_output(df_for_line(), input$medicine, input$date_range, input$variable, rv)
        })
    
    # output$infotext <- renderText({"The <b>[variable]</b> of <b>[drug]</b>
    #     prescribed in <b>[area] [increased/decreased]</b> <b>[x]%</b> between
    #     <b>[date 1]</b> and <b>[date2]</b>. Average monthly prescribing across
    #     this period was <b>[x]% [above/below]</b> the national average."})
    
    # Note: if we want to use caching, RDT server must be set to False
}

