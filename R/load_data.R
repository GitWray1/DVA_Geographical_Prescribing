
# Load libraries ----------------------------------------------------------

library(dplyr)


# Load in data ------------------------------------------------------------

#data_file <- "data/output_files/DOACs_data_long.csv"
#data_file <- "data/test_data/DOACs_data_nhs_manc_edox_0.csv"
data_file <- "data/test_data/DOACs_data_nhs_manc_edox_na.csv"

df <- readr::read_csv(data_file)

if (is.character(df$date)) {
    df <- df %>% 
        mutate(date = lubridate::as_date(date, format = "%d/%m/%Y"))
}

# Load in shapefiles ------------------------------------------------------

# Create list of file paths
shape_files <- list.files(path= "data/output_files/simplified_geojsons", 
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

