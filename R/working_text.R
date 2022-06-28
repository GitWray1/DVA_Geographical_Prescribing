

# Functions to create text output -----------------------------------------

# Function to get area name 
get_area_name <- function(df, rv) {
    if (!is.null(rv$click)) {
        temp <- unique(df[df$ods_code == rv$click[1], "name"])
    } else {
        temp <- "England"
    }
    return(temp)
}

# Function to return the % change when given a start and end value
get_percent_change <- function(start_val, end_val) {
    temp <- (end_val/start_val-1)*100
    return(temp)
}

# Function to return the change direction text given the % change
get_change_direction <- function(percent_change = 0) {
    temp <- case_when(
        percent_change > 0 ~ "an increase",
        percent_change < 0 ~ "a decrease",
        percent_change == 0 ~ "didn't change",
        TRUE ~ "Error, possible 0 or missing value")
    return(temp)
}

# Aggregate area totals for each date, or if area clicked that specific area only
get_time_aggregate_df <- function(df, input_dates, clicked_area = NULL) {
    if (is.null(clicked_area)) {
        temp <- df %>% 
            filter(date >= input_dates[1],
                   date <= input_dates[2])
    } else {
        temp <- df %>% 
            filter(date >= input_dates[1],
                   date <= input_dates[2],
                   ods_code == clicked_area)
    }
    
    temp <- temp %>% 
        group_by(date) %>% 
        summarise(across(c(registered_patients, items, quantity, actual_cost), sum, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate("items_per_1000" = (items/(registered_patients/1000)),
               "quantity_per_1000" = (quantity/(registered_patients/1000)),
               "actual_cost_per_1000" = (actual_cost/(registered_patients/1000)))
    return(temp)
}

# 
get_nat_average <- function(df, input_variable) {
    temp <- df %>% 
    select(input_variable) %>% 
    summarise(across(everything(), mean, na.rm = TRUE)) %>% 
    as.numeric()
    return(temp)
}

# Calculate average for area
get_area_sum <- function(df, input_variable) {
    temp <- df %>% 
        select(input_variable) %>% 
        as.numeric()
    return(temp)
}

get_output_text <- function(df, input_med, input_dates, input_variable, rv) {
    
    browser()
    time_aggregated_df <- get_time_aggregate_df(df, input_dates, clicked_area = NULL)
    
    nat_average <- get_nat_average(time_aggregated_df, input_variable)
    
    if (!is.null(rv$click)) {
      
        area_aggregated_df <- get_time_aggregate_df(df, input_variable, clicked_area = rv$click[1])
        
        area_sum <- get_area_sum(area_aggregated, input_variable)
        
    }
    
    # Extract each variable needed for the dynamic text 
        
    start_val <- ifelse(is.null(rv$click), 
                        time_aggregated_df[time_aggregated_df$date == input_dates[1], input_variable], 
                        area_aggregated_df[area_aggregated_df$date == input_dates[1], input_variable]) %>% 
                 as.numeric()
    
    end_val <- ifelse(is.null(rv$click), 
                        time_aggregated_df[time_aggregated_df$date == input_dates[2], input_variable], 
                        area_aggregated_df[area_aggregated_df$date == input_dates[2], input_variable]) %>% 
                as.numeric()
        
    area_name <- get_area_name(df, rv)
    percent_change <- get_percent_change(start_val, end_val)
    change_direction <- get_change_direction(percent_change)
    
    # Build the text
    output_text <- paste0("The <b>", stringr::str_to_lower(get_y_title(input_variable)), "</b> of <b>", input_med, 
                          "</b> prescribed in <b> ", area_name, "</b> changed from <b>", tidy_number(start_val), "</b> in <b>", 
                          tidy_date(input_dates[1]), "</b> to <b>", tidy_number(end_val), "</b> in <b>", tidy_date(input_dates[2]), "</b>")
    
    if (percent_change != 0){
        output_text <- paste0(output_text, ", ", change_direction, " of <b>", round(percent_change,2), "%</b>.")
        
    } else {
        output_text <- paste0(output_text, ".<b> No change</b> was observed over the period.")
    }
    return(output_text)
}


# Create text output function ---------------------------------------------

create_text_output <- function(df, input_med, input_dates, input_variable, rv) {
    
    output_text <- get_output_text(df, input_med, input_dates, input_variable, rv)
    
    # if (is.null(rv$click)){ # No area selected
    # 
    #     output_text <- get_output_text(df, input_med, input_dates, input_variable, rv)
    # 
    # } else { # Area selected
    # 
    #     output_text <- get_output_text(df, input_med, input_dates, input_variable, rv)
    #     # add second sentence
    # }
    
    return(output_text)
}
