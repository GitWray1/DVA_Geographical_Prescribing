
# Load packages -----------------------------------------------------------

library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(leaflet)


# Load data ---------------------------------------------------------------
# This is messy - maybe split this out into a separate 'loading data' files and source it in
# Also create a file with some functions e.g. filter_for_line_chart(), create_map() etc...

data_file <- "1_data_prep/output_files/DOACs_data.csv"

df <- readr::read_csv(data_file)

# Example data for data table tab
df_small <- df %>% 
    mutate(date = format(date, "%b %Y")) %>% 
    select(date, chemical, bnf_code, 
           ccg_name, ccg_ods, ccg_gss, registered_patients, 
           items, quantity, actual_cost)


shape_file <- "1_data_prep/output_files/simplified_geojsons/simplified_CCGs_(April_2021)_EN_BFC.geojson"

ccg_shape_df <- sf::st_read(shape_file, as_tibble = TRUE) %>%  
    select(-OBJECTID) %>% 
    janitor::clean_names()

col_df <- df %>% 
    select(date, chemical, ccg_name, ccg_ods, ccg_gss, registered_patients, items, quantity, actual_cost) %>% 
    filter(chemical == "Apixaban",
           date == "2022-01-01")

# Example data for leaflet output
data_shp_df <- left_join(ccg_shape_df, col_df,
                         by = c("ccg21cd" = "ccg_gss",
                                "ccg21nm" = "ccg_name")) %>% 
    dplyr::rename("ccg_gss" = "ccg21cd",
                  "ccg_name" = "ccg21nm") %>% 
    mutate(items_per_1000 = items/(registered_patients/1000))


# Read in shapefiles -  temporary solution - need to clean up code

files <- list.files(path= "1_data_prep/output_files/simplified_geojsons", pattern = "*.geojson", full.names = TRUE)

shp_files <- tibble()

for (i in seq_along(files)) {
    
    temp <- st_read(files[i], as_tibble = TRUE) %>% janitor::clean_names()
    
    if (!is.null(temp$global_id)) {
        temp$global_id <- NULL
    }
    
    colnames(temp) <- c("object_id", "gss_code", "name", "bng_e", "bng_n", "long", "lat", "shape_area", "shape_length", "geometry")
    
    temp <- temp %>% select(-object_id)
    shp_files <- rbind(shp_files,temp)
    
}


# Set colour pallete to use
pal <- colorNumeric("plasma", domain = NULL)



# Create Shiny UI ---------------------------------------------------------

ui <- bootstrapPage(
        navbarPage(title = "Geographical Prescribing Dashboard",
                   theme = shinytheme("flatly"),
                   collapsible = TRUE,
                   
          tabPanel("Prescribing map",
                   div(class="outer",
                       tags$head(includeCSS("styles.css")),
                       
                       leafletOutput("mymap", width = "100%", height = "100%"),
                       
                       absolutePanel(id = "filterpanel", class = "panel panel-default", fixed = TRUE,
                                     draggable = TRUE, height = "auto", width = "270",
                                     top = 100, bottom = "auto", left = "50", right = "auto",
                                     
                                     selectInput("medicine", 
                                                 "Select medicine",
                                                 choices = unique(df$chemical),
                                                 selected = unique(df$chemical)[1]),
                                     
                                     hr(),
                                     
                                     selectInput("variable",
                                                 "Select variable to plot",
                                                 choices = c("Items" = "items",
                                                             "Quantity" = "quantity",
                                                             "Actual cost" = "actual_cost"),
                                                             #"Items per 1000 registered people" = "items_per_1000",
                                                             #"Quantity per 1000 registered people" = "quantity_per_1000",
                                                             #"Actual cost per 1000 registered people" = "actual_cost_per_1000"),
                                                 selected = "items"),
                                    
                                     hr(),
                                     
                                     radioGroupButtons("area",
                                                       "NHS geographical area",
                                                       choices = c("Region" = "region", 
                                                                   "STP" = "stp", 
                                                                   "CCG" = "ccg"),
                                                       selected = "ccg",
                                                       size = "sm",
                                                       justified = TRUE),
                                     
                                     hr(),
                                     
                                     airMonthpickerInput("date_range",
                                                         "Select date range",
                                                         range = TRUE,
                                                         value = c(lubridate::as_date(max(df$date)) - lubridate::period("1 year"),
                                                                   lubridate::as_date(max(df$date))),
                                                         dateFormat = "M yyyy",
                                                         minDate = lubridate::as_date(min(df$date)),
                                                         maxDate = lubridate::as_date(max(df$date)),
                                                         clearButton = TRUE,
                                                         autoClose = TRUE),
                                    
                                    hr(),
                                        
                                    dateRangeInput("date_range3",
                                                   "Select date range",
                                                   start = lubridate::as_date(min(df$date)),
                                                   end = lubridate::as_date(max(df$date)),
                                                   startview = "year",
                                                   format = "M yyyy"),
                                    
                                    hr()
                       ))),
          
          tabPanel("Data",
                   DT::dataTableOutput("data_table"),
                   downloadButton("full_csv_download",
                                  "Download as CSV")),
          
          tabPanel("Data test",
                   DT::dataTableOutput("test_table")),
          
          tabPanel("About this dashboard",
                   includeMarkdown("about.rmd"))
          )
        )
        
