

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

get_output_text <- function(df, input_med, input_dates, input_variable, rv) {
    
    # National average for medicine for each month, across all areas
    time_aggregated_df <- get_time_aggregate_df(df, input_dates, clicked_area = NULL)
    
    # Mean across all months for variable of interest
    nat_average <- sum(time_aggregated_df[[input_variable]]/106, na.rm = TRUE)
    
    if (!is.null(rv$click)) {
        # Monthly average for clicked area
        area_aggregated_df <- get_time_aggregate_df(df, input_dates, clicked_area = rv$click[1])
        
        # Mean sum across 
        area_sum <- sum(area_aggregated_df[[input_variable]], na.rm = TRUE)
    
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
        
    area_name <- as.character(get_area_name(df, rv))
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
    
    # if (!is.null(rv$click)){
    #     
    #     diff_percentage <- get_percent_change(nat_average, area_sum)
    #     
    #     # Calculate increase/decrease
    #     change_direction <- case_when(
    #         diff_percentage > 0 ~ paste0("was <b>", tidy_number(diff_percentage), "%</b> " ,"above the national average."),
    #         diff_percentage < 0 ~ paste0("was <b>", tidy_number(abs(diff_percentage)), "%</b> " ,"below the national average."),
    #         diff_percentage == 0 ~ " <b>did not differ</b> from the national average.",
    #         TRUE ~ "Error, possible 0 or missing value.")
    #     
    #     # Add the final sentence
    #     output_text <- paste0(output_text, " Total prescribing of <b>", input_med,"</b> across this period ", change_direction)
    # }
    
    return(output_text)
}

