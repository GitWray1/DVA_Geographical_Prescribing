
# Load packages -----------------------------------------------------------

library(dplyr)
library(leaflet)
library(plotly) 

# Import data -------------------------------------------------------------

# File locations
data_file <- "1_data_prep/output_files/DOACs_data.csv"
shape_file <- "1_data_prep/output_files/simplified_geojsons/simplified_CCGs_(April_2021)_EN_BFC.geojson"

# Read in data

df <- readr::read_csv(data_file)

# Read in CCG shapefile
ccg_shape_df <- sf::st_read(shape_file, as_tibble = TRUE) %>%  
                        select(-OBJECTID) %>% 
                        janitor::clean_names()

# Line chart - using plotly -----------------------------------------------

# Filter for line chart 
line_df <- df %>% 
    select(date, chemical, registered_patients, items, quantity, actual_cost) %>% 
    group_by(date, chemical) %>% 
    summarise("nat_list_size" = sum(registered_patients),
              "nat_items" = sum(items),
              "nat_quantity" = sum(quantity),
              "actual_cost" = sum(actual_cost)) %>% 
    ungroup()

# Create line chart

line_df %>%
    plot_ly(x = ~date, 
            y = ~nat_items, 
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

# Filter for column chart
col_df <- df %>% 
    select(date, chemical, ccg_name, ccg_ods, ccg_gss, registered_patients, items, quantity, actual_cost) %>% 
    filter(chemical == "Apixaban",
           date == "2022-01-01")

# Create chart
col_df %>% 
    plot_ly(x = ~reorder(ccg_ods, items),
            y = ~items,
            type = "bar",
            hoverinfo = "text",
            hovertext = paste0("Name: ", col_df$ccg_name,
                               "<br>ODS code: ", col_df$ccg_ods,
                               "<br>GSS code: ", col_df$ccg_gss,
                               "<br>Items: ", col_df$items)) %>% 
    layout(title = list(text = "<b>Example chart title</b>",
                        x = "0.1"),
           yaxis = list(title = "<b>Example Y-axis Title</b>",
                        tickformat = ",",
                        rangemode = "tozero"),
           xaxis = list(title = FALSE,
                        showticklabels = FALSE),
           hovermode = "x unified")


# Create leaflet map ------------------------------------------------------

# Join data to shapefiles dataframe
data_shp_df <- left_join(ccg_shape_df, col_df,
                         by = c("ccg21cd" = "ccg_gss",
                                "ccg21nm" = "ccg_name")) %>% 
                dplyr::rename("ccg_gss" = "ccg21cd",
                              "ccg_name" = "ccg21nm") %>% 
                mutate(items_per_1000 = items/(registered_patients/1000))

# Set colour pallete to use
pal <- colorNumeric("plasma", domain = NULL)

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
                label = ~lapply(paste0("<strong>Name: </strong>", data_shp_df$ccg_name,
                                       "<br><strong>Items per 1000:</strong> ", round(data_shp_df$items_per_1000, 2)), 
                                htmltools::HTML),
                labelOptions = labelOptions(textsize = "12px",
                                            style = list("font-family" = "Arial")))



# Working area ------------------------------------------------------------

test <- df %>% select(date, chemical, bnf_code, starts_with("ccg"),
              registered_patients, items, quantity, actual_cost) %>% 
    filter(chemical == "Apixaban") %>% 
    group_by(date, chemical, bnf_code, across(starts_with("ccg"))) %>% 
    summarise("registered_patients" = sum(registered_patients), 
              "items" = sum(items), 
              "quantity" = sum(quantity), 
              "actual_cost" = sum(actual_cost)) %>% 
    ungroup()

test %>% 
    filter(date >= "2020-01-01",
           date <= "2020-12-01") %>% 
    group_by(chemical, bnf_code, across(starts_with("ccg"))) %>% 
    summarise("items" = sum(items),
              "quantity" = sum(quantity), 
              "actual_cost" = sum(actual_cost))
