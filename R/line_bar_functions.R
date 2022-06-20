
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
    
    yaxis <- get_y_title(rv_var)
    
    temp <- paste0(yaxis, " of ", input_med, " prescribed in England over the last 5 years")
    return(temp)
}

# Get title for bar chart 
get_bar_title <- function(rv_var, input_med, input_area, input_dates){
    
    yaxis <- get_y_title(rv_var)
    
    date1 <- tidy_date(input_dates[1])
    date2 <- tidy_date(input_dates[2])
    
    temp <- paste0("Total ",stringr::str_to_lower(yaxis), " of ", input_med, " prescribed between ", date1, 
                   " and ", date2, " by ", input_area)
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

create_line_chart <- function(line_df, input_variable, input_dates, rv, input_med){
    
    p <- line_df %>%
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
        layout(title = list(text = stringr::str_wrap(paste0("<b>", get_line_title(input_variable, input_med),"</b>"), width = 80),
                            font = list(size = 12),
                            x = 0.05,
                            yanchor = "bottom"),
               yaxis = list(title = get_y_title(input_variable),
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
    
    if (!is.null(rv$click)) {
        
        temp_df <- line_df %>% 
            filter(ods_code == rv$click[1])
        
        p <- p %>% 
            add_trace(data = temp_df,
                      y = ~tidy_number(temp_df[[input_variable]]),
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
    return(p)
}


# Create bar chart --------------------------------------------------------

create_bar_chart <- function(bar_df, input_variable, rv, input_dates, input_med, input_area){
    
    bar_df <- bar_df %>% arrange(get(input_variable))
    
    click_index <- which(bar_df$ods_code == rv$click[1]) - 1
    
    median_var <- median(bar_df[[input_variable]], na.rm = TRUE)
    
    temp <- bar_df %>%
        plot_ly(x = ~reorder(ods_code, get(input_variable)),
                y = ~get(input_variable),
                type = "bar",
                hoverinfo = "text",
                hovertext = paste0("Name: ", bar_df$name,
                                   "<br>ODS code: ", bar_df$ods_code,
                                   "<br>GSS code: ", bar_df$gss_code,
                                   "<br>", get_y_title(input_variable), ": ", tidy_number(bar_df[[input_variable]])),
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
                  hovertext = paste0("National Median: ", tidy_number(median_var))) %>%
        add_annotations(xref = "paper",
                        yref ="y",
                        x = 0.05,
                        y = median_var,
                        yshift = 9,
                        text = "<b>National median</b>",
                        showarrow = FALSE,
                        font = list(color = '#393939',
                                    size = 12)) %>% 
        layout(yaxis = list(title = get_y_title(input_variable),
                            tickformat = ",",
                            rangemode = "tozero"),
               xaxis = list(title = list(text = paste0("<b>", get_tidy_area(input_area),"s</b>"),
                                         font = list(size=12),
                                         standoff = 10),
                            showticklabels = FALSE),
               hovermode = "x unified",
               title = list(text = stringr::str_wrap(paste0("<b>", get_bar_title(input_variable, input_med, get_tidy_area(input_area), input_dates),"</b>"), width = 78),
                            font = list(size = 12),
                            x = 0.05,
                            yanchor = "bottom")) %>% 
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoom", "pan", "select", "lasso"))
    return(temp)
}


