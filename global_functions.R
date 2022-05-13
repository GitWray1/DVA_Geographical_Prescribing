
# Set up extra bits -------------------------------------------------------

# Set colour pallete to use
pal <- colorNumeric("plasma", domain = NULL)
#qpal <- colorQuantile("plasma", domain = NULL, n = 5, na.color = "#808080")


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


# Functions to get reactive text ------------------------------------------

# Function to format dates into 'Mon-YYYY' format
format_date <- function(input_date){
    temp <- format(lubridate::as_date(input_date), "%b %Y")
    return(temp)
}

# format_number <- function(number){
#     temp <- format(number, big.mark = ",", nsmall = 1, scientific = FALSE)
#     return(temp)
# }

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
    
    date1 <- format(lubridate::as_date(input_dates[1]), "%b %Y")
    date2 <- format(lubridate::as_date(input_dates[2]), "%b %Y")
    
    temp <- paste0(rv_var, " of ", input_med, " prescribed between ", date1, 
                   " and ", date2, " by ", input_area)
    return(temp)
}
# use format date function for above

# Creating the reactive text output

create_text_output <- function(df, input_med, date_range, input_variable, rv){

    date1 <- format_date(date_range[1])
    date2 <- format_date(date_range[2])


    if (is.null(rv$click)){ # No area selected

        temp_df <- df %>% filter(date >= date_range[1],
                                 date <= date_range[2]) %>% 
            group_by(date) %>% 
            summarise(across(c(registered_patients, items, quantity, actual_cost), sum)) %>% 
            mutate("items_per_1000" = (items/(registered_patients/1000)),
                   "quantity_per_1000" = (quantity/(registered_patients/1000)),
                   "actual_cost_per_1000" = (actual_cost/(registered_patients/1000))) %>% 
            select(date, input_variable)
        
        start_val <- temp_df[temp_df$date == date_range[1],input_variable]
        end_val <- temp_df[temp_df$date == date_range[2],input_variable]
        
        # Consider division by 0 or missing
        diff_percentage <- (end_val/start_val-1)*100
        
        change_direction <- case_when(
                                diff_percentage > 0 ~ "an increase",
                                diff_percentage < 0 ~ "a decrease",
                                diff_percentage == 0 ~ "didn't change",
                                TRUE ~ "Error, possible 0 or missing value")

        temp <- paste0("The ", rv$yaxis, " of ", input_med, " prescribed in England", " changed from ", scales::comma(as.numeric(start_val)), 
                       " on ", date1, " to ", scales::comma(as.numeric(end_val)), " on ", date2)
        
        if (diff_percentage != 0){
            temp2 <- paste0(temp, ", ", change_direction, " of ", round(diff_percentage,2), "%.")
        } else {
            temp2 <- paste0(temp, ". No change was observed over the period.")
        }
        
                          
    } else { # Area selected

        start_val <- df[(df$date == date_range[1] & df$ods_code == rv$click[1]), input_variable]
        end_val <- df[(df$date == date_range[2] & df$ods_code == rv$click[1]), input_variable]
        
        # Consider division by 0 or missing
        diff_percentage <- (end_val/start_val-1)*100
        
        change_direction <- case_when(
            diff_percentage > 0 ~ "an increase",
            diff_percentage < 0 ~ "a decrease",
            diff_percentage == 0 ~ "didn't change",
            TRUE ~ "Error, possible 0 or missing value")
        
        temp <- paste0("The ", rv$yaxis, " of ", input_med, " prescribed in ", 
                       df[(df$date == date_range[1] & df$ods_code == rv$click[1]), "name"], " changed from ",
                       scales::comma(as.numeric(start_val)), " on ", date1, " to ", scales::comma(as.numeric(end_val)), " on ", date2)
        
        if (diff_percentage != 0){
            temp2 <- paste0(temp, ", ", change_direction, " of ", round(diff_percentage,2), "%.")
        } else {
            temp2 <- paste0(temp, ". No change was observed over the period.")
        }
        
        
        # Calculate national average
        nat_average <- df %>% 
            filter(date >= date_range[1],
                   date <= date_range[2]) %>% 
            group_by(name) %>% 
            summarise(across(c(registered_patients, items, quantity, actual_cost), sum, na.rm = TRUE)) %>% 
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
            mutate("items_per_1000" = (items/(registered_patients/1000)),
                   "quantity_per_1000" = (quantity/(registered_patients/1000)),
                   "actual_cost_per_1000" = (actual_cost/(registered_patients/1000))) %>% 
            select(input_variable) %>% as.numeric()
        
        # calculate % difference
        diff_percentage <- (area_sum/nat_average-1)*100
        
        # Calculate increase/decrease
        change_direction <- case_when(
            diff_percentage > 0 ~ paste0("was ", scales::comma(as.numeric(diff_percentage), accuracy = 0.1),"% " ,"above the national average."),
            diff_percentage < 0 ~ paste0("was ", scales::comma(as.numeric(abs(diff_percentage)), accuracy = 0.1),"% " ,"below the national average."),
            diff_percentage == 0 ~ " did not differ from the national average.",
            TRUE ~ "Error, possible 0 or missing value")
        
        
        temp2 <- paste0(temp2, " Total prescribing of ", input_med," across this period ", change_direction)
    }

    return(temp2)
}



# e.g.The [variable] of [drug name] prescribed in [selected area] 
# from [x items] on [date 1] to [x items] on [date2], a [x]% [increase/decrease].
# Total prescribing across this period was [x]% [above/below] the national average. 


# # # df_for_line e.g.# 
# temp_df <- df %>% filter(chemical == "Apixaban",
#               area_type == "ccg") %>%
#     mutate("items_per_1000" = (items/(registered_patients/1000)),
#            "quantity_per_1000" = (quantity/(registered_patients/1000)),
#            "actual_cost_per_1000" = (actual_cost/(registered_patients/1000)))


              