
# Load libraries ----------------------------------------------------------

library(dplyr)


# Load in data ------------------------------------------------------------

#data_file <- "1_data_prep/output_files/DOACs_data.csv"
data_file <- "1_data_prep/output_files/DOACs_data_long.csv"

df <- readr::read_csv(data_file)

# Load in shapefiles ------------------------------------------------------

# Create list of file paths
shape_files <- list.files(path= "1_data_prep/output_files/simplified_geojsons", 
                          pattern = "*.geojson", 
                          full.names = TRUE)

# Initialize shapefile dataframe
shp_files <- tibble()

# Read in each shapefile, make consistent add to shapefile dataframe
for (i in seq_along(shape_files)) {
    
    temp <- sf::st_read(shape_files[i], as_tibble = TRUE) %>% 
        janitor::clean_names() %>% 
        select(ends_with(c("cd", "nm")), bng_e, bng_n, long, lat, contains("shape"), geometry)
    
    colnames(temp) <- c("gss_code", "name", "bng_e", "bng_n", "long", "lat", "shape_area", "shape_length", "geometry")
    
    shp_files <- rbind(shp_files, temp)
}


# Create example cuts for figures -----------------------------------------

# Example data for data table tab
df_small <- df %>% 
    mutate(date = format(date, "%b %Y")) %>% 
    select(date, chemical, bnf_code, area_type, 
           name, ods_code, gss_code, registered_patients, 
           items, quantity, actual_cost)

# Example data for line chart 
line_df <- df %>% 
    filter(area_type == "ccg") %>% 
    select(date, chemical, registered_patients, items, quantity, actual_cost) %>% 
    group_by(date, chemical) %>% 
    summarise("nat_list_size" = sum(registered_patients),
              "nat_items" = sum(items),
              "nat_quantity" = sum(quantity),
              "actual_cost" = sum(actual_cost)) %>% 
    ungroup()

# Example data for bar chart
bar_chart_df <- df %>%
    select(date, chemical, area_type, name, ods_code, gss_code, registered_patients, items, quantity, actual_cost) %>% 
    filter(chemical == "Apixaban",
           date == "2022-01-01",
           area_type == "ccg")

# Example data for leaflet output
data_shp_df <- shp_files %>% 
    filter(stringr::str_detect(name, "CCG")) %>% 
    left_join(bar_chart_df) %>% 
    mutate(items_per_1000 = items/(registered_patients/1000))
