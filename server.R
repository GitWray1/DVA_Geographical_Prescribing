
# Load packages -----------------------------------------------------------

library(shiny)
library(dplyr)

# Create server  ----------------------------------------------------------

server <- function(input, output, session) {
    
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
    
    # When the area is changed return the rv$click value to NULL
    observeEvent(input$area, {
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
    
    # Update bar chart title and store as reactive variable
    observeEvent(c(rv$yaxis, input$medicine, input$area, input$date_range), {
        rv$bar_title <- get_bar_title(rv$yaxis, input$medicine, rv$area, input$date_range)
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
        
        #qpal <- colorQuantile("plasma", domain = df_for_map()[[input$variable]], n = 5, na.color = "#808080")
        
        # qpal_colors <- unique(qpal(sort(df_for_map()[[input$variable]]))) # hex codes
        # qpal_labs <- quantile(df_for_map()[[input$variable]], seq(0, 1, .2)) # depends on n from pal
        # qpal_labs <- paste(round(lag(qpal_labs)), round(qpal_labs), sep = " - ")[-1] # first lag is NA
        
        pal <- colorBin("plasma", 
                        domain = df_for_map()[[input$variable]],
                        bins = 5,
                        na.color = "#808080")
        #pal <- colorNumeric("plasma", domain = df_for_map()[[input$variable]])
        
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
                                               "<br><strong>", rv$yaxis,":</strong> ", tidy_number(df_for_map()[[input$variable]])),
                                        htmltools::HTML),
                        labelOptions = labelOptions(textsize = "12px",
                                                    style = list("font-family" = "Arial"))) %>%
            clearControls() %>%
            addLegend(position = "bottomleft",
                      pal = pal,
                      #colors = qpal_colors,
                      #labels = qpal_labs,
                      values = ~df_for_map()[[input$variable]],
                      title = rv$yaxis,
                      opacity = 0.7)
    })
    

# Create the line chart ---------------------------------------------------
    
    output$line_chart <- renderPlotly({
        
        date_vlines <- list(
                        list(type = "line",
                             fillcolor = "black",
                             line = list(color = "black",
                                         dash = "dot",
                                         width = 1.5),
                             opacity = 0.3,
                             x0 = input$date_range[1],
                             x1 = input$date_range[1],
                             xref = "x",
                             y0 = 0,
                             y1 = 0.95,
                             yref = "paper"),
            
                        list(type = "line",
                             fillcolor = "black",
                             line = list(color = "black",
                                         dash = "dot",
                                         width = 1.5),
                             opacity = 0.3,
                             x0 = input$date_range[2],
                             x1 = input$date_range[2],
                             xref = "x",
                             y0 = 0,
                             y1 = 0.95,
                             yref = "paper"))
        

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
                    hoverinfo = "name+x+y",
                    name = 'National Average',
                    marker = list(color = "#004650",
                                  size = 4)) %>%
            add_ribbons(ymin = ~round(lower_ci, 2),
                        ymax = ~round(upper_ci, 2),
                        fillcolor = 'rgba(0,70,80,0.2)',
                        line = list(color = 'transparent'),
                        marker = list(color = "transparent"),
                        name = '95% CI',
                        hoverinfo = "text",
                        hovertext = "95% Confidence interval") %>% 
            # add_trace(x = ~as.Date("2020-01-01"),
            #           yref = "paper",
            #           y0 = 0,
            #           y1 = 1,
            #           type = 'scatter', 
            #           mode = 'lines',
            #           line = list(color = 'black'),
            #           name = '') %>% 
            layout(title = list(text = stringr::str_wrap(paste0("<b>", rv$line_title,"</b>"), width = 80),
                                font = list(size = 12),
                                x = 0.05,
                                yanchor = "bottom"),
                   yaxis = list(title = rv$yaxis,
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
                   shapes = date_vlines) %>% 
            config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("zoom", "pan", "select", "lasso"))
        
       if (!is.null(rv$click)) {
           
           temp_df <- df_for_line() %>% 
                            filter(ods_code == rv$click[1])
           
           p %>% 
               add_trace(data = temp_df,
                         y = ~tidy_number(temp_df[[input$variable]]),
                         type = "scatter",
                         mode = "lines+markers",
                         line = list(color = "#D5824D"),
                         connectgaps = TRUE,
                         name = ~stringr::str_trunc(name, width = 30, side = "right"),
                         marker = list(color = "#D5824D",
                                       size = 4))
               
       } else {
           
           p
       }
       
    })
    

# Create the bar chart ----------------------------------------------------

    # Use plotly proxy?
    output$bar_chart <- renderPlotly({
        
        ind <- which(df_for_bar()$ods_code == rv$click[1]) - 1
        
        df_for_bar() %>%
            plot_ly(x = ~reorder(ods_code, get(input$variable)),
                    y = ~get(input$variable),
                    type = "bar",
                    hoverinfo = "text",
                    hovertext = paste0("Name: ", df_for_bar()$name,
                                       "<br>ODS code: ", df_for_bar()$ods_code,
                                       "<br>GSS code: ", df_for_bar()$gss_code,
                                       "<br>", rv$yaxis, ": ", tidy_number(df_for_bar()[[input$variable]])),
                    showlegend = FALSE,
                    selectedpoints = c(106, ind),
                    color = I("#004650"),
                    alpha = 0.6,
                    selected = list(marker = list(color ="#D5824D")),
                    unselected = list(marker = list(color ="#004650",
                                                    opacity = 0.6))) %>%
            add_lines(y = ~median(get(input$variable), na.rm = TRUE),
                      name = "National Median",
                      line = list(dash = 'dot',
                                  width = 2,
                                  color = "#393939"),
                      hovertext = paste0("National Median: ", tidy_number(median(df_for_bar()[[input$variable]])))) %>%
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
                   title = list(text = stringr::str_wrap(paste0("<b>", rv$bar_title,"</b>"), width = 78),
                                font = list(size = 12),
                                x = 0.05,
                                yanchor = "bottom")) %>% 
            config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("zoom", "pan", "select", "lasso"))
 
    })
    
    output$infotext <- renderText({
        create_text_output(df_for_line(), input$medicine, input$date_range, input$variable, rv)
        })
    
    
    # Note: if we want to use caching, RDT server must be set to False
}

