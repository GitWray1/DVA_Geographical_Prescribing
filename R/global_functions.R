
# Generic functions -------------------------------------------------------

# Function to format dates into 'Mon-YYYY' format
tidy_date <- function(input_date){
    temp <- format(lubridate::as_date(input_date),
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
get_y_title <- function(input_var){
    
    temp <- case_when(
        input_var == "items" ~ "Items",
        input_var == "quantity" ~ "Quantity",
        input_var == "actual_cost" ~ "Actual Cost",
        input_var == "items_per_1000" ~ "Items per 1000 people",
        input_var == "quantity_per_1000" ~ "Quantity per 1000 people",
        input_var == "actual_cost_per_1000" ~ "Actual cost per 1000 people",
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

create_text_output <- function(df, input_med, date_range, input_variable, rv){

    date1 <- tidy_date(date_range[1])
    date2 <- tidy_date(date_range[2])

    if (is.null(rv$click)){ # No area selected

        # Wrangle df to get national data across the selected period
        temp_df <- df %>% 
                    filter(date >= date_range[1],
                           date <= date_range[2]) %>% 
                    group_by(date) %>% 
                    summarise(across(c(registered_patients, items, quantity, actual_cost), sum)) %>% 
                    ungroup() %>% 
                    mutate("items_per_1000" = (items/(registered_patients/1000)),
                           "quantity_per_1000" = (quantity/(registered_patients/1000)),
                           "actual_cost_per_1000" = (actual_cost/(registered_patients/1000))) %>% 
                    select(date, input_variable)
        
        # Extract each variable needed for the dynamic text 
        start_val <- temp_df[temp_df$date == date_range[1],input_variable]
        end_val <- temp_df[temp_df$date == date_range[2],input_variable]
        
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
        area_name <- df[(df$date == date_range[1] & df$ods_code == rv$click[1]), "name"]
        start_val <- df[(df$date == date_range[1] & df$ods_code == rv$click[1]), input_variable]
        end_val <- df[(df$date == date_range[2] & df$ods_code == rv$click[1]), input_variable]
        
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
                filter(date >= date_range[1],
                       date <= date_range[2]) %>% 
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
                filter(date >= date_range[1],
                       date <= date_range[2],
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



# Functions to make line chart --------------------------------------------

#' Filter data frame for line chart and calculate values per 1000 registered people
#'
#' @param df The full data frame
#' @param medicine_input The medicine to plot
#' @param area_input The geographical area to plot, options = c("region","stp","ccg")
#'
#' @return The data frame filtered for selected medicine and geographical area, 
#' with additional values calculated per 1000 registered people
#' 
#' @export
#'
#' @examples
#' filter_for_line(df, "Apixaban", "ccg")
#' filter_for_line(df, "Edoxaban", "region")
filter_for_line <- function(df, medicine_input, area_input){
    
    temp <- df %>% 
        
        
        return(temp)
}

              