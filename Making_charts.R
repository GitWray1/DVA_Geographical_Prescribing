
# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(sf)
library(leaflet)
library(janitor)

# Import data -------------------------------------------------------------

df <- read_csv("1_data_prep/output_files/DOACs_data.csv")


df %>% 
  select(date, chemical, total_list_size, items, quantity, actual_cost) %>% 
  filter(chemical == "Apixaban") %>% 
  group_by(date, chemical) %>% 
  summarise("nat_list_size" = sum(total_list_size),
            "nat_items" = sum(items),
            "nat_quantity" = sum(quantity),
            "actual_cost" = sum(actual_cost)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(x = date, y = nat_items), size = 1, colour = "#004650") + 
  theme_light() +
  scale_x_date(date_breaks = "6 month", date_labels = "%b %Y") +
  theme(plot.title = element_text(size=14),
        axis.title.y = element_text(size=12, vjust = 2),
        axis.text.x = element_text(size=12, hjust=1),
        axis.text.y = element_text(size=12),
        axis.title.x = element_blank()) +
  labs(title = "test",
       y = "test",
       caption = "Source: test")



make_plot <- reactive({
  
  filtered() %>% filter(HE_var %in% input$cbox) %>% 
    ggplot(aes(YEAR_MONTH, n)) + 
    geom_line(aes(colour = HE_var), size = 1.5) + theme_light() +
    scale_x_date(date_breaks = "2 month", date_labels = "%Y-%m") +
    theme(legend.title = element_blank(),
          plot.title = element_text(size=24, face="bold"),
          legend.text = element_text(size=16),
          axis.title.y = element_text(size=16, vjust = 2),
          axis.text.x = element_text(size=16, angle = 45, hjust=1),
          axis.text.y = element_text(size=16)) +
    labs(title = "Myocardial Infarctions",
         x = element_blank(),
         y = "Number of Myocardial Infarctions")
})

