
# Generic functions -------------------------------------------------------

# Function to format dates into 'Mon-YYYY' format
tidy_date <- function(date){
    temp <- format(lubridate::as_date(date),
                   "%b %Y")
    return(temp)
}

# Clean up numbers (round, add commas etc)
tidy_number <- function(number){
    temp <- format(as.numeric(number),
                   big.mark = ",",
                   digits = 2,
                   nsmall = 2, 
                   scientific = FALSE,
                   drop0trailing = TRUE)
    return(temp)
}


# Functions for chart labels and titles -----------------------------------

# Get titles for line and bar chart y axes
get_y_title <- function(input_variable){
    
    temp <- case_when(
        input_variable == "items" ~ "Items",
        input_variable == "quantity" ~ "Quantity",
        input_variable == "actual_cost" ~ "Actual Cost",
        input_variable == "items_per_1000" ~ "Items per 1000 people",
        input_variable == "quantity_per_1000" ~ "Quantity per 1000 people",
        input_variable == "actual_cost_per_1000" ~ "Actual cost per 1000 people",
        TRUE ~ "Unknown variable"
    )
    return(temp)
}

# Get title for line chart
get_line_title <- function(rv_var, input_med){
    
    temp <- paste0(rv_var, " of ", input_med, " prescribed in England over the last 5 years")
    return(temp)
}

# Get title for bar chart 
get_bar_title <- function(rv_var, input_med, input_area, input_dates){
    
    date1 <- tidy_date(input_dates[1])
    date2 <- tidy_date(input_dates[2])
    
    temp <- paste0("Total ",stringr::str_to_lower(rv_var), " of ", input_med, " prescribed between ", date1, 
                   " and ", date2, " by ", input_area)
    return(temp)
}


# Functions to get reactive text ------------------------------------------

create_text_output <- function(df, input_med, input_dates, input_variable, rv){

    date1 <- tidy_date(input_dates[1])
    date2 <- tidy_date(input_dates[2])

    if (is.null(rv$click)){ # No area selected

        # Wrangle df to get national data across the selected period
        temp_df <- df %>% 
                    filter(date >= input_dates[1],
                           date <= input_dates[2]) %>% 
                    group_by(date) %>% 
                    summarise(across(c(registered_patients, items, quantity, actual_cost), sum)) %>% 
                    ungroup() %>% 
                    mutate("items_per_1000" = (items/(registered_patients/1000)),
                           "quantity_per_1000" = (quantity/(registered_patients/1000)),
                           "actual_cost_per_1000" = (actual_cost/(registered_patients/1000))) %>% 
                    select(date, input_variable)
        
        # Extract each variable needed for the dynamic text 
        start_val <- temp_df[temp_df$date == input_dates[1],input_variable]
        end_val <- temp_df[temp_df$date == input_dates[2],input_variable]
        
        diff_percentage <- (end_val/start_val-1)*100
        
        change_direction <- case_when(
                                diff_percentage > 0 ~ "an increase",
                                diff_percentage < 0 ~ "a decrease",
                                diff_percentage == 0 ~ "didn't change",
                                TRUE ~ "Error, possible 0 or missing value")

        # Build the text
        output_text <- paste0("The <b>", stringr::str_to_lower(rv$yaxis), "</b> of <b>", input_med, "</b> prescribed in England", " changed from <b>", tidy_number(start_val), 
                       "</b> in <b>", date1, "</b> to <b>", tidy_number(end_val), "</b> in <b>", date2, "</b>")
        
        if (diff_percentage != 0){
            output_text <- paste0(output_text, ", ", change_direction, " of <b>", round(diff_percentage,2), "%</b>.")
            
        } else {
            output_text <- paste0(output_text, ".<b> No change</b> was observed over the period.")
        }
        
                          
    } else { # Area selected

        # Extract each variable needed for the dynamic text (data at local level so no need to summarise)
        area_name <- df[(df$date == input_dates[1] & df$ods_code == rv$click[1]), "name"]
        start_val <- df[(df$date == input_dates[1] & df$ods_code == rv$click[1]), input_variable]
        end_val <- df[(df$date == input_dates[2] & df$ods_code == rv$click[1]), input_variable]
        
        diff_percentage <- (end_val/start_val-1)*100
        
        change_direction <- case_when(
                                diff_percentage > 0 ~ "an increase",
                                diff_percentage < 0 ~ "a decrease",
                                diff_percentage == 0 ~ "didn't change",
                                TRUE ~ "Error, possible 0 or missing value")
        
        # Build the dynamic text
        output_text <- paste0("The <b>", stringr::str_to_lower(rv$yaxis), "</b> of <b>", input_med, "</b> prescribed in <b>", 
                       area_name, "</b> changed from <b>", tidy_number(start_val), "</b> on <b>", 
                       date1, "</b> to <b>", tidy_number(end_val), "</b> on <b>", date2,"</b>")
        
        if (diff_percentage != 0){
            output_text <- paste0(output_text, ", ", change_direction, " of <b>", tidy_number(diff_percentage), "%</b>.")
            
        } else {
            output_text <- paste0(output_text, ". No change was observed over the period.")
        }
        
        # For the final sentence we want to calculate the difference from the national average across the period
        # Calculate national average
        nat_average <- df %>% 
                filter(date >= input_dates[1],
                       date <= input_dates[2]) %>% 
                group_by(name) %>% 
                summarise(across(c(registered_patients, items, quantity, actual_cost), sum, na.rm = TRUE)) %>%
                ungroup() %>% 
                mutate("items_per_1000" = (items/(registered_patients/1000)),
                       "quantity_per_1000" = (quantity/(registered_patients/1000)),
                       "actual_cost_per_1000" = (actual_cost/(registered_patients/1000))) %>% 
                select(input_variable) %>% 
                summarise(across(everything(), mean)) %>% 
                as.numeric()
        
        # Calculate average for area
        area_sum <- df %>% 
                filter(date >= input_dates[1],
                       date <= input_dates[2],
                       ods_code == rv$click[1]) %>% 
                group_by(name) %>% 
                summarise(across(c(registered_patients, items, quantity, actual_cost), sum, na.rm = TRUE)) %>%
                ungroup() %>% 
                mutate("items_per_1000" = (items/(registered_patients/1000)),
                       "quantity_per_1000" = (quantity/(registered_patients/1000)),
                       "actual_cost_per_1000" = (actual_cost/(registered_patients/1000))) %>% 
                select(input_variable) %>% as.numeric()
        
        # calculate % difference
        diff_percentage <- (area_sum/nat_average-1)*100
        
        # Calculate increase/decrease
        change_direction <- case_when(
                diff_percentage > 0 ~ paste0("was <b>", tidy_number(diff_percentage), "%</b> " ,"above the national average."),
                diff_percentage < 0 ~ paste0("was <b>", tidy_number(abs(diff_percentage)), "%</b> " ,"below the national average."),
                diff_percentage == 0 ~ " <b>did not differ</b> from the national average.",
                TRUE ~ "Error, possible 0 or missing value.")
        
        # Add the final sentence
        output_text <- paste0(output_text, " Total prescribing of <b>", input_med,"</b> across this period ", change_direction)
    }
    
    return(output_text)
}


# Functions to filter data ------------------------------------------------

filter_for_line <- function(df, input_med, input_area){

    temp <- df %>% 
                filter(chemical == input_med,
                       area_type == input_area) %>%
                mutate(items_per_1000 = (items/(registered_patients/1000)),
                       quantity_per_1000 = (quantity/(registered_patients/1000)),
                       actual_cost_per_1000 = (actual_cost/(registered_patients/1000)))
    return(temp)
}

filter_for_bar <- function(line_df, input_dates){
    
    temp <- line_df %>% 
                filter(date >= input_dates[1],
                       date <= input_dates[2]) %>% 
                group_by(chemical, bnf_code, name, ods_code, gss_code) %>% 
                summarise(registered_patients = sum(registered_patients),
                          items = sum(items), 
                          quantity = sum(quantity), 
                          actual_cost = sum(actual_cost)) %>% 
                ungroup() %>% 
                mutate(items_per_1000 = (items/(registered_patients/1000)),
                       quantity_per_1000 = (quantity/(registered_patients/1000)),
                       actual_cost_per_1000 = (actual_cost/(registered_patients/1000))) %>% 
                select(-registered_patients)
    return(temp)
}


# Functions to make map ---------------------------------------------------

create_start_map <- function(){
    
    temp <- leaflet(options = leafletOptions(zoomDelta = 0.25,
                                             zoomSnap = 0.25)) %>% 
            setView(lat = 53,
                    lng = 0,
                    zoom = 6.75) %>%
            addProviderTiles(provider = "CartoDB.Positron")
    return(temp)
}

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
                                           "<br><strong>", rv$yaxis,":</strong> ", tidy_number(map_df[[input_variable]])),
                                    htmltools::HTML),
                    labelOptions = labelOptions(textsize = "12px",
                                                style = list("font-family" = "Arial"))) %>%
        clearControls() %>%
        addLegend(position = "bottomleft",
                  pal = pal,
                  values = ~map_df[[input_variable]],
                  title = rv$yaxis,
                  opacity = 0.7)
    return(temp)
}


# Create line chart -------------------------------------------------------

# Function to define vertical date lines on chart
define_vline <- function(input_date){
    
    temp <- list(type = "line",
                 fillcolor = "black",
                 line = list(color = "black",
                             dash = "dot",
                             width = 2.5),
                 opacity = 0.3,
                 x0 = input_date,
                 x1 = input_date,
                 xref = "x",
                 y0 = 0,
                 y1 = 0.95,
                 yref = "paper")
    return(temp)
}

# Function to make line chart

create_line_chart <- function(line_df, input_variable, input_dates, rv){
    
    temp <- line_df %>%
        group_by(date, chemical, bnf_code) %>% 
        summarise(mean_variable = weighted.mean(get(input_variable), registered_patients, na.rm = TRUE),
                  sd_variable = sqrt(Hmisc::wtd.var(get(input_variable), registered_patients, na.rm = TRUE)),
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
               shapes = list(define_vline(input_dates[1]), 
                             define_vline(input_dates[2]))) %>% 
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoom", "pan", "select", "lasso"))
    return(temp)
}


# Create bar chart --------------------------------------------------------

create_bar_chart <- function(bar_df, input_variable, rv){
    
    click_index <- which(bar_df$ods_code == rv$click[1]) - 1
    
    temp <- bar_df %>%
        plot_ly(x = ~reorder(ods_code, get(input_variable)),
                y = ~get(input_variable),
                type = "bar",
                hoverinfo = "text",
                hovertext = paste0("Name: ", bar_df$name,
                                   "<br>ODS code: ", bar_df$ods_code,
                                   "<br>GSS code: ", bar_df$gss_code,
                                   "<br>", rv$yaxis, ": ", tidy_number(bar_df[[input_variable]])),
                showlegend = FALSE,
                selectedpoints = c(106, click_index),
                color = I("#004650"),
                alpha = 0.6,
                selected = list(marker = list(color ="#D5824D")),
                unselected = list(marker = list(color ="#004650",
                                                opacity = 0.6))) %>%
        add_lines(y = ~median(get(input_variable), na.rm = TRUE),
                  name = "National Median",
                  line = list(dash = 'dot',
                              width = 2,
                              color = "#393939"),
                  hovertext = paste0("National Median: ", tidy_number(median(bar_df[[input_variable]])))) %>%
        add_annotations(xref = "paper",
                        yref ="y",
                        x = 0.05,
                        y = median(bar_df[[input_variable]], na.rm = TRUE),
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
    return(temp)
}

#input_med, input_variable, input_med, input_area, input_dates
              