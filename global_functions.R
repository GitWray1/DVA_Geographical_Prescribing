
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

# Creating the reactive text output

# create_text_output <- function(df, input_med, date_range, variable, rv){
# 
#     date1 <- format_date(date_range[1], "%b %Y")
#     date2 <- format_date(date_range[2], "%b %Y")
# 
# 
#     if (is.null(click_event)){ # No area selected
# 
#         temp_df <- 
#         start_val <-
#         end_val <-
#         diff_percentage <- (end_val/start_val-1)*100
#         change_direction <- case_when(
#                                 diff_percentage > 0 ~ "increased",
#                                 diff_percentage < 0 ~ "decreased",
#                                 TRUE ~ "didn't change")
# 
#         temp <- paste0("The ", rv$yaxis, " of ", input_med, "prescribed in England", change_direction,
#                        " by ", diff_percentage, "% between ", date1, " and ", date2, ".")
# 
# 
#     } else { # Area selected
# 
#     }
# 
#     return(temp)
# }


# e.g.The [variable] of [drug name] prescribed in [selected area] [increased/decreased] 
# [x]% between [date 1] and [date2]. Total prescribing across this period was [x]% [above/below] 
# the national average. 


# # df_for_line e.g.# 
# temp_df <- df %>% filter(chemical == "Apixaban",
#               area_type == "ccg") %>%
#     mutate("items_per_1000" = (items/(registered_patients/1000)),
#            "quantity_per_1000" = (quantity/(registered_patients/1000)),
#            "actual_cost_per_1000" = (actual_cost/(registered_patients/1000)))

