
# Exploring recreating the leaflet in plotly. Leaflet doesnt have a non-html download.
# Exploring whether we could add a download button that would download the recreated plotly map.

# Load packages -----------------------------------------------------------

library(dplyr)
library(sf)
library(plotly)


# Load data ---------------------------------------------------------------

# Temp line df
temp_line_df <- df %>% filter(chemical == "Apixaban",
                              area_type == "ccg") %>%
                       mutate("items_per_1000" = (items/(registered_patients/1000)),
                              "quantity_per_1000" = (quantity/(registered_patients/1000)),
                              "actual_cost_per_1000" = (actual_cost/(registered_patients/1000)))

# Temp bar df
temp_bar_df <- temp_line_df %>%
    filter(date >= "2021-01-01",
           date <= "2021-12-01") %>%
    group_by(chemical, bnf_code, name, ods_code, gss_code) %>%
    summarise("registered_patients" = sum(registered_patients),
              "items" = sum(items),
              "quantity" = sum(quantity),
              "actual_cost" = sum(actual_cost)) %>%
    ungroup() %>%
    mutate("items_per_1000" = (items/(registered_patients/1000)),
           "quantity_per_1000" = (quantity/(registered_patients/1000)),
           "actual_cost_per_1000" = (actual_cost/(registered_patients/1000))) %>%
    select(-registered_patients)

# Temp map df
temp_map_df <- shp_files %>% inner_join(temp_bar_df)

# Make map ----------------------------------------------------------------

plot_ly(temp_map_df) %>% 
  add_sf(type = "scatter",
         stroke = I("black"),
         span = I(1),
         alpha = 0.4,
         split = ~ods_code,
         color = ~cut(items, breaks = 5),
         hoverinfo = "none",
         colors = "plasma",
         showlegend = FALSE
         )

# Trying to make same bins as leaflet
temp_map_df2 <- temp_map_df %>% 
  mutate(new_bin = cut(items, breaks = pretty(items, n = 5), dig.lab = 7))               

plot_ly(temp_map_df2) %>% 
  add_sf(type = "scatter",
         stroke = I("black"),
         span = I(1),
         alpha = 0.3,
         split = ~ods_code,
         color = ~new_bin,
         hoverinfo = "none",
         colors = "plasma",
         showlegend = FALSE
  ) %>% 
  colorbar(title = "hello")





