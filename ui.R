
# Load packages -----------------------------------------------------------

library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(leaflet)


# Source in other files ---------------------------------------------------

source("loading_data.R")
#source("global_functions.R")


# Set up extra bits -------------------------------------------------------

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
                                                             "Actual cost" = "actual_cost",
                                                             "Items per 1000 people" = "items_per_1000",
                                                             "Quantity per 1000 people" = "quantity_per_1000",
                                                             "Actual cost per 1000 people" = "actual_cost_per_1000"),
                                                 selected = "items"),
                                    
                                     hr(),
                                     
                                     radioGroupButtons("area",
                                                       "NHS geographical area",
                                                       choices = c("Region" = "region", 
                                                                   "STP" = "stp", 
                                                                   "CCG" = "ccg"),
                                                       selected = "ccg",
                                                       status = "primary",
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
        
