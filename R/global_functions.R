
# Generic functions -------------------------------------------------------

# Function to format dates into 'Mon-YYYY' format
tidy_date <- function(date){
    temp <- format(lubridate::as_date(date),
                   "%b %Y")
    return(temp)
}

# Clean up numbers (round, add commas etc)
tidy_number <- function(number){
    temp <- format(as.numeric(number),
                   big.mark = ",",
                   digits = 2,
                   nsmall = 2, 
                   scientific = FALSE,
                   drop0trailing = TRUE)
    return(temp)
}

#input_med, input_variable, input_med, input_area, input_dates

              