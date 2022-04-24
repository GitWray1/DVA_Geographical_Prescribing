
# Load packages -----------------------------------------------------------

library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinythemes)


# Load data ---------------------------------------------------------------

# Strangly, app doesnt run until these two lines are run first
data_file <- "1_data_prep/output_files/DOACs_data.csv"

df <- readr::read_csv(data_file)

# Create Shiny UI ---------------------------------------------------------

ui <- bootstrapPage(
        navbarPage(title = "Geographical Prescribing Dashboard",
                   theme = shinytheme("flatly"),
                   collapsible = TRUE,
                   
          tabPanel("Prescribing map"),
          
          tabPanel("Data",
                   DT::dataTableOutput("data_table"),
                   downloadButton("full_csv_download",
                                  "Download as CSV")),
          
          tabPanel("About this dashboard",
                   includeMarkdown("about.rmd"))
          )
        )
        
