
# geoJSONS are downloaded from ONS Open Geography Portal. These contain complex 
# geometries so need to be simplified to greatly increase plotting speed. Have 
# used rmapshaper instead of st_simplify() because it preserves shared borders.

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(sf) # Spatial features
library(rmapshaper)


# Load and simplify geoJSONs -----------------------------------------------

# List files paths for full geojsons in input directory
files <- list.files(path= "1_data_prep/input_files/geojsons", pattern = "*.geojson", full.names = TRUE)

# Set output directory for simplified files
output_dir = "1_data_prep/output_files/simplified_geojsons/"

# Loop through files - load file, simplify and save to output directory
# Slow to run so added print() to track progress
for (i in seq_along(files)) {
  
  temp_df <- st_read(files[[i]], as_tibble = TRUE) %>% 
                ms_simplify(keep = 0.05, keep_shapes = FALSE)
  st_write(temp_df, str_c(output_dir, "simplified_", basename(files[[i]])))
  print(str_c("Done (",i,"/", length(files),"): ", files[[i]]))
}

