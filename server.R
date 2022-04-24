library(shiny)
library(dplyr)


server <- function(input, output, session) {
    
    df_small <- df %>% select(date, chemical, bnf_code, 
                              ccg_name, ccg_ods, registered_patients, 
                              items, quantity, actual_cost)
    
    output$data_table <- DT::renderDataTable(df_small,
                                             filter = "top",
                                             options = list(autoWidth = TRUE,
                                                            columnDefs = list(list(width = '400px',
                                                                                   targets = 4))))
    # Note: if we want to use caching, RDT server must be set to False

}