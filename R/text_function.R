
# Function to get reactive text ------------------------------------------

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
        output_text <- paste0("The <b>", stringr::str_to_lower(get_y_title(input_variable)), "</b> of <b>", input_med, "</b> prescribed in England", " changed from <b>", tidy_number(start_val), 
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
        output_text <- paste0("The <b>", stringr::str_to_lower(get_y_title(input_variable)), "</b> of <b>", input_med, "</b> prescribed in <b>", 
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