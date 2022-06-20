get_nat_average_df <- function(df, input_dates, input_variable){
    temp <- df %>% 
        filter(date >= input_dates[1],
               date <= input_dates[2]) %>% 
        group_by(date) %>% 
        summarise(across(c(registered_patients, items, quantity, actual_cost), sum)) %>% 
        ungroup() %>% 
        mutate("items_per_1000" = (items/(registered_patients/1000)),
               "quantity_per_1000" = (quantity/(registered_patients/1000)),
               "actual_cost_per_1000" = (actual_cost/(registered_patients/1000))) %>% 
        select(date, input_variable)
    return(temp)
}

get_value <- function(df, input_variable, input_date){
    temp <- df[df$date == input_date, input_variable]
    return(temp)
}

get_percent_change <- function(start_val, end_val) {
    temp <- (end_val/start_val-1)*100
    return(temp)
}

get_change_direction <- function(diff_percentage) {
    temp <- case_when(
        diff_percentage > 0 ~ "an increase",
        diff_percentage < 0 ~ "a decrease",
        diff_percentage == 0 ~ "didn't change",
        TRUE ~ "Error, possible 0 or missing value")
    return(temp)
}

get_no_area_text <- function(df, input_med, input_dates, input_variable) {
    
    nat_avg_df <- get_nat_average_df(df, input_dates, input_variable)
    
    # Extract each variable needed for the dynamic text 
    start_val <- get_value(nat_avg_df, input_variable, input_dates[1])
    end_val <- get_value(nat_avg_df, input_variable, input_dates[2])
    percent_change <- get_percent_change(start_val, end_val)
    change_direction <- get_change_direction(percent_change)
    
    # Build the text
    output_text <- paste0("The <b>", stringr::str_to_lower(get_y_title(input_variable)), "</b> of <b>", input_med, 
                          "</b> prescribed in England", " changed from <b>", tidy_number(start_val), "</b> in <b>", 
                          tidy_date(input_dates[1]), "</b> to <b>", tidy_number(end_val), "</b> in <b>", tidy_date(input_dates[2]), "</b>")
    
    if (diff_percentage != 0){
        output_text <- paste0(output_text, ", ", change_direction, " of <b>", round(diff_percentage,2), "%</b>.")
        
    } else {
        output_text <- paste0(output_text, ".<b> No change</b> was observed over the period.")
    }
    return(output_text)
}

create_text_output <- function(df, input_med, input_dates, input_variable, rv){
    
    if (is.null(rv$click)){ # No area selected
        
        output_text <- get_no_area_text(df, input_med, input_dates, input_variable)
        
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
        output_text <- paste0("The <b>", stringr::str_to_lower(get_y_title(input_variable)), "</b> of <b>", input_med, "</b> prescribed in <b>", 
                              area_name, "</b> changed from <b>", tidy_number(start_val), "</b> on <b>", 
                              tidy_date(input_dates[1]), "</b> to <b>", tidy_number(end_val), "</b> on <b>", tidy_date(input_dates[2]),"</b>")
        
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
