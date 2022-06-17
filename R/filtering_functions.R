
# Filter for line chart ---------------------------------------------------

#' filter_for_line
#' Filter data for line chart
#' 
#' Function to filter the underlying data according to the user inputs, which
#' can then be plotted in a line chart
#'
#' @param df data frame with the following columns ...
#' @param input_med user selected medicine
#' @param input_area user selected area
#'
#' @examples
#' filter_for_line(df, "Apixaban", "ccg")
#' filter_for_line(df, "Warfarin sodium", "region")
#' @author Jonathan Wray

filter_for_line <- function(df, input_med, input_area){
    
    # check that the column names (above) are in the dataframe
    names <- c("chemical", "area_type", "items", "quantity", "actual_cost", "registered_patients")
    if (sum(names %in% colnames(df)) != length(names)) {
        stop("Dataframe must contain correct column names")
    }
    # Check that inputs have at least 1 valid row
    if (!input_med %in% unique(df$chemical)){
        stop("Dataframe does not contain selected medicine")
    }
    if (!input_area %in% unique(df$area_type)){
        stop("Dataframe does not contain selected area type")
    }
    
    temp <- df %>% 
        filter(chemical == input_med,
               area_type == input_area) %>%
        mutate(items_per_1000 = (items/(registered_patients/1000)),
               quantity_per_1000 = (quantity/(registered_patients/1000)),
               actual_cost_per_1000 = (actual_cost/(registered_patients/1000)))
    return(temp)
}


# Filter for bar chart ----------------------------------------------------

#' filter_for__bar
#' Filter data for bar chart
#' 
#' Function to further filter the data used in the line chart according to the 
#' user inputs, which can then be plotted in a bar chart
#'
#' @param line_df data frame used for line chart
#' @param input_dates user selected vector containing a start and end date
#'
#' @examples
#' filter_for_bar(filter_for_line(), c("2020-01-01", "2020-12-31"))
#' filter_for_bar(filter_for_line(), c("2021-01-01", "2020-06-30"))
#' @author Jonathan Wray

filter_for_bar <- function(line_df, input_dates){
    # check that df has all names needed
    names <- c("date","chemical", "bnf_code", "name", "ods_code", "gss_code", 
               "items", "quantity", "actual_cost", "registered_patients")
    if (sum(names %in% colnames(line_df)) != length(names)) {
        stop("Dataframe must contain correct column names")
    }
    
    temp <- line_df %>% 
        filter(date >= input_dates[1],
               date <= input_dates[2]) %>% 
        group_by(chemical, bnf_code, name, ods_code, gss_code) %>% 
        summarise(registered_patients = sum(registered_patients),
                  items = sum(items), 
                  quantity = sum(quantity), 
                  actual_cost = sum(actual_cost)) %>% 
        ungroup() %>% 
        mutate(items_per_1000 = (items/(registered_patients/1000)),
               quantity_per_1000 = (quantity/(registered_patients/1000)),
               actual_cost_per_1000 = (actual_cost/(registered_patients/1000))) %>% 
        select(-registered_patients)
    return(temp)
}


# Filter for map ----------------------------------------------------------

#' filter_for_map
#' Filter data for leaflet map
#' 
#' Function to join the filtered data frame to the shape files, which can then 
#' be plotted in the leaflet map
#'
#' @param bar_df data frame used for bar chart
#' @param shape_files geojson dataframe containing area shapes
#'
#' @examples
#' filter_for_map(filter_for_bar(), shp_files)
#' @author Jonathan Wray

filter_for_map <- function(bar_df, shape_files_df){
    
    # Check shape_files_df is correct type - has geometry column
    if (!"sf" %in% class(shape_files_df)) {
        stop("shape_files_df must have a geometry column (sf)")
    }
    # Check that they share a column for the join
    if (!sum(colnames(bar_df) %in% colnames(shape_files_df) > 0)) {
        stop("bar_df and shape_files_df must have a shared column")
    }
    
    temp <- shp_files %>% inner_join(bar_df)
    return(temp)
}