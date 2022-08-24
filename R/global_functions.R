
# Function to convert dates -----------------------------------------------

#' tidy_date
#' Tidy date format
#' 
#' Function to format dates into 'Mon YYYY' format
#'
#' @param date date in format "yyyy-mm-dd"
#'
#' @examples
#' tidy_date("2020-01-01") ## returns "Jan 2020"
#' tidy_date("2021-06-15") ## returns "Jun 2021"
#' @author Jonathan Wray

tidy_date <- function(date){
    # if (!is.vector(date)) {
    #     stop("Input must be a vector") # makes tests fail if included - dates not vector?
    #}
    if (length(date) == 0) {
        stop("Input vector must be non-zero length")
    }
    if(!class(date) %in% c("character", "Date")){
        stop("Input must be either in character or date format")
    }
    if(!tryCatch(lubridate::is.Date(lubridate::as_date(date)),
                 warning = function(w) return(FALSE))) {
        stop("Check date is valid and in format 'yyyy-mm-dd'")
    }
    # used tryCatch as simple if statement returned NAs and warnings, 
    # these can be read as FALSE with trycatch. Also handles numbers.

    temp <- format(lubridate::as_date(date), "%b %Y")
    
    return(temp)
}


# Function to tidy numbers ------------------------------------------------

#' tidy_number
#' Tidy number format
#'
#' Rounds numbers to 2 decimal places, adds commas for thousands, removes trailing 0s 
#' and returns as a character
#'
#' @param number integer or float 
#'
#' @examples
#' tidy_number(1500L) ## returns "1,500"
#' tidy_number(10000.534) ## returns "10,000.53"
#' @author Jonathan Wray

tidy_number <- function(number){
    
    if (length(number) == 0) {
        stop("Input vector must of non-zero length")
    }
    if(!is.numeric(number)) {
        number <- as.character(number)
    }
    
    temp <- format(as.numeric(number),
                   big.mark = ",",
                   digits = 2,
                   nsmall = 2, 
                   scientific = FALSE,
                   drop0trailing = TRUE)
    return(temp)
}

# Get tidy area name ------------------------------------------------------

#' get_tidy_area
#' Get input area in clean format
#'
#' Takes input and capitalises as appropriate to be used in free text and figures
#'
#' @param character vector with options ("ccg", "stp", "Region")
#'
#' @examples
#' get_tidy_area("ccg") ## returns "CCG"
#' get_tidy_area("region") ## returns "Region"
#' @author Jonathan Wray

get_tidy_area <- function(input_area) {
    
    if (length(input_area) == 0) {
        stop("Input vector must be non-zero length")
    }
    if(!class(input_area) == "character") {
        stop("Input must be character format")
    }
    # Haven't specified that area should be either ccg, stp or region as this is 
    # covered by the case_when below
    temp <- case_when(input_area == "ccg" ~ "CCG",
                      input_area == "stp" ~ "STP",
                      input_area == "region" ~ "Region",
                      TRUE ~ "Unknown area input")
    
    return(temp)
}
              