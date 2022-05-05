
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

# Functions to make bar chart ---------------------------------------------

#filter_for_bar <- function(df, ){}

# Functions to make map ---------------------------------------------------

# filter_for_map <- function(df, ){}