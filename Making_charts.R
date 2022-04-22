
# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(sf)
library(leaflet)
library(janitor)
library(plotly)

# Import data -------------------------------------------------------------

df <- read_csv("1_data_prep/output_files/DOACs_data.csv")


# Line chart - using plotly -----------------------------------------------

# Filter for line chart 
line_df <- df %>% 
    select(date, chemical, total_list_size, items, quantity, actual_cost) %>% 
    #filter(chemical == "Apixaban") %>% 
    group_by(date, chemical) %>% 
    summarise("nat_list_size" = sum(total_list_size),
              "nat_items" = sum(items),
              "nat_quantity" = sum(quantity),
              "actual_cost" = sum(actual_cost)) %>% 
    ungroup()

# Create line chart
line_df %>%
    plot_ly(x = ~date, 
            y = ~nat_items, 
            type = "scatter", 
            mode = "lines+markers",
            color = ~chemical) %>% 
    layout(title = list(text = "<b>Example chart title</b>",
                        x = 0.1),
           yaxis = list(title = "<b>Example Y-axis Title</b>",
                        tickformat = ",",
                        rangemode = "tozero"),
           xaxis = list(title = FALSE,
                        type = 'date',
                        tickformat = "%b<br>%Y"),
           hovermode = "x unified",
           legend = list(title = list(text = "Medicine"), 
                         x = 100, 
                         y = 0.5))


# Bar chart ---------------------------------------------------------------

# Filter for column chart
col_df <- df %>% 
    select(date, chemical, ccg_name, ccg_ods, ccg_gss, total_list_size, items, quantity, actual_cost) %>% 
    filter(chemical == "Apixaban",
           date == "2022-01-01")

# Create chart
col_df %>% 
    plot_ly(x = ~reorder(ccg_ods, items),
            y = ~items,
            type = "bar",
            hoverinfo = "text",
            hovertext = paste0("Name: ", col_df$ccg_name,
                               "<br>ODS code: ", col_df$ccg_ods,
                               "<br>GSS code: ", col_df$ccg_gss,
                               "<br>Items: ", col_df$items)) %>% 
    layout(title = list(text = "<b>Example chart title</b>",
                        x = "0.1"),
           yaxis = list(title = "<b>Example Y-axis Title</b>",
                        tickformat = ",",
                        rangemode = "tozero"),
           xaxis = list(title = FALSE,
                        showticklabels = FALSE),
           hovermode = "x unified")


# Create leaflet map ------------------------------------------------------




