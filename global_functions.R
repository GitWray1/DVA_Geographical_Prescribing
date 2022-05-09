
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


# Function to make text output --------------------------------------------



# create_text_output <- function(df, medicine, date_range, variable, rv){
#     
# if (!is.null(rv$val)){ # area selected
#     
#     start_val <- df %>% filter(ods_code == rv$val,
#                                  date == date_range[1]) %>% 
#                           select(variable) %>% as.double()
#     end_val <- df %>% filter(ods_code == rv$val,
#                       date == date_range[2]) %>% 
#                       select(variable) %>% as.double()
#     sum_over_period <- df %>% filter(ods_code == rv$val,
#                                      date >= date_range[1],
#                                      date <= date_range[2]) %>% 
#                               
#     nat_avg_sum_over_period <- 
#     area <- rv$val
#     
#     temp <- paste0("The ", variable, " of ", medicine, " prescribed in ", area, " ", 
#                    change_direction, " ", change_percent, "% between ", date1,
#                    " and ", date2, ".")
#     
# } else { # no area selected
#     
#     temp <- paste0("The ", variable, " of ", medicine, " prescribed in England ", 
#                    change_direction, " ", change_percent, "% between ", date1,
#                    " and ", date2, ".")
# }
#     med <- "Apixaban"
#     var <- 
#     change_direction <- "increased"
#     change_percent <- "8"
#     area <- "England"
#     date1 <- format(lubridate::as_date("2020-01-01"), "%b %Y")
#     date2 <- format(lubridate::as_date("2021-01-01"), format = "%b %Y")
#     
#     temp <- paste0("The ", variable, " of ", medicine, " prescribed in ", area, " ", 
#                    change_direction, " ", change_percent, "% between ", date1,
#                    " and ", date2, ".")
#     return(temp)
# }


# e.g.The [variable] of [drug name] prescribed in [selected area] [increased/decreased] 
# [x]% between [date 1] and [date2]. Total prescribing was across this period [x]% [above/below] 
# the national average. 

# The items of Apixaban prescribed between ...
# The quantity of Apixaban prescribed between ...
# The items per 1000 population of Apixaban prescribed in  ...
