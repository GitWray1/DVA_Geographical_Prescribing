
# Load packages -----------------------------------------------------------

library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(leaflet)
library(plotly)


# Source in other files ---------------------------------------------------

source("R/load_data.R")
source("R/filtering_functions.R")
source("R/global_functions.R")
source("R/map_functions.R")
source("R/line_bar_functions.R")
source("R/new_text_function.R")

# Create Shiny UI ---------------------------------------------------------

ui <- bootstrapPage(
        navbarPage(title = "Geographical Prescribing Dashboard",
                   theme = shinytheme("flatly"),
                   collapsible = TRUE,
                   
          tabPanel("Prescribing map",
                   div(class="outer",
                       tags$head(includeCSS("www/styles.css")),
                       
                       leafletOutput("mymap", width = "100%", height = "100%"),
                       
                       absolutePanel(id = "infopanel", class = "panel panel-default", fixed = TRUE,
                                     draggable = TRUE, height = "auto", width = "500",
                                     top = "130", bottom = "auto", left = "20", right = "auto",

                                     htmlOutput("infotext"),

                                     ),
                       
                       absolutePanel(id = "filterpanel", class = "panel panel-default", fixed = TRUE,
                                     draggable = TRUE, height = "auto", width = "270",
                                     top = "300", bottom = "auto", left = "20", right = "auto",
                                     
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
                                                       status = "myClass",
                                                       size = "sm",
                                                       justified = TRUE),
                                     
                                     hr(),
                                     
                                     airMonthpickerInput("date_range",
                                                         "Select date range",
                                                         range = TRUE,
                                                         value = c(lubridate::as_date(max(df$date)) - lubridate::period("11 months"),
                                                                   lubridate::as_date(max(df$date))),
                                                         dateFormat = "M yyyy",
                                                         minDate = lubridate::as_date(min(df$date)),
                                                         maxDate = lubridate::as_date(max(df$date)),
                                                         minView = "months",
                                                         autoClose = TRUE,
                                                         toggleSelected = FALSE,
                                                         addon = "left"),
                                     
                                     hr()
                                     
                                     #downloadButton("mapdownload", "Download map")
                                        
                       ),
                       
                       absolutePanel(id = "outputpanel", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, height = "auto", width = "600",
                                      top = 70, bottom = "auto", left = "auto", right = "10",
                                     
                                     h4("Click on an area for further details "),
                                     
                                     br(),
                                     
                                     plotlyOutput("line_chart", height = "300px", width = "auto"),
                                     
                                     hr(),
                                     
                                     plotlyOutput("bar_chart", height = "300px", width = "auto"),
                                     
                                     uiOutput("clear_click", align = "center"))
                       
                       )),
          
          tabPanel("Data",
                   DT::dataTableOutput("data_table"),
                   downloadButton("download_full_csv",
                                  "Download full dataset"),
                   downloadButton("download_cut_csv",
                                  "Download filtered data")
                   ),
          
          tabPanel("About this dashboard",
                   includeMarkdown("R/about.rmd"))
          )
        )
        
