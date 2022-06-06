
# Load packages -----------------------------------------------------------

library(shiny)
library(dplyr)


# Create server  ----------------------------------------------------------

server <- function(input, output, session) {

    
# Filter the data according to user input ---------------------------------
    
    df_for_line <- reactive({
        filter_for_line(df, input$medicine, input$area)
    })
    
    df_for_bar <- reactive({
        filter_for_bar(df_for_line(), input$date_range)
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

    output$mymap <- renderLeaflet(create_start_map())
    
    observeEvent(c(df_for_map(), input$variable), {
        update_map(df_for_map(), input$variable, pal, rv)
    })
    

# Create the line chart ---------------------------------------------------
    
    output$line_chart <- renderPlotly({
        create_line_chart(df_for_line(), input$variable, input$date_range, rv)
    })
    

# Create the bar chart ----------------------------------------------------

    output$bar_chart <- renderPlotly({
        create_bar_chart(df_for_bar(), input$variable, rv)
    })


# Render reactive text ----------------------------------------------------

    output$infotext <- renderText({
        create_text_output(df_for_line(), input$medicine, input$date_range, input$variable, rv)
        })
    

# Exporting figures and data ----------------------------------------------

    # Download the full data set without filters
    output$download_full_csv <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), "_full_data.csv")
        },
        content = function(file) {
            readr::write_csv(df, file)
        }
    )
    
    # Download the data as filtered by the user in the data tab
    output$download_cut_csv <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), "_filtered_data.csv")
        },
        content = function(file) {
            readr::write_csv(df[input[["data_table_rows_all"]], ], file)
        }
    )
    
    # library(mapview)
    # map_download_test <- reactive({
    #     leaflet(options = leafletOptions(zoomDelta = 0.25,
    #                                      zoomSnap = 0.25)) %>% 
    #         setView(lat = 53,
    #                 lng = 0,
    #                 zoom = 6.75) %>%
    #         addProviderTiles(provider = "CartoDB.Positron")})
    
    # output$mapdownload <- downloadHandler(filename = paste0(Sys.Date(), "_custom_map.png"),
    # 
    #                                       content = function(file){
    #                                           mapshot(x = map_object(),
    #                                                   file = file,
    #                                                   cliprect = "viewport",
    #                                                   selfcontained = FALSE)
    #                                       })
    
    # output$mapdownload <- downloadHandler(filename = paste0(Sys.Date(), "_custom_map.html"),
    #                                       content = function(file){
    #                                           htmlwidgets::saveWidget(
    #                                               widget = map_download_test(),
    #                                               file = file)
    #                                           })

}

