
# Load packages -----------------------------------------------------------

library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(leaflet)
library(plotly)


# Source in other files ---------------------------------------------------

source("loading_data.R")
source("global_functions.R")


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
                                     top = "150", bottom = "auto", left = "40", right = "auto",
                                     
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
                                    
                                    textOutput("testtext"),
                                        
                       ),
                       
                       absolutePanel(id = "outputpanel", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, height = "auto", width = "600",
                                      top = 70, bottom = "auto", left = "auto", right = "10",
                                     
                                     h4("Use the left panel to select parameters for plotting"),
                                     
                                     h5("Click on an area for further details "),
                                     
                                     htmlOutput("infotext"),
                                     
                                     br(),
                                     
                                     plotlyOutput("line_chart", height = "300px", width = "auto"),
                                     
                                     hr(),
                                     
                                     plotlyOutput("bar_chart", height = "300px", width = "auto"),
                                     
                                     uiOutput("clear_click", align = "center"))
                       
                       )),
          
          tabPanel("Data",
                   DT::dataTableOutput("data_table"),
                   downloadButton("full_csv_download",
                                  "Download as CSV")),
          
          tabPanel("About this dashboard",
                   includeMarkdown("about.rmd"))
          )
        )
        
