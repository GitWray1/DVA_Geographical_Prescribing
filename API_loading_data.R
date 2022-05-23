

# Load Packages -----------------------------------------------------------

library(dplyr)


# Load Data ---------------------------------------------------------------

bnf_code_lookup <- readr::read_csv("1_data_prep/input_files/BNF_code_lookup.csv") %>% 
                        janitor::clean_names()


# Create search -----------------------------------------------------------

build_url <- function(url_bnf_code){
    url_base <- "https://openprescribing.net/api/1.0/spending_by_ccg/?code="
    url_end <- "&format=csv"
    csv_url <- paste0(url_base,
                      url_bnf_code,
                      url_end)
    return(csv_url)
}

test_df1 <- readr::read_csv(build_url("0208020Z0"))

test_df2 <- readr::read_csv(build_url("0208020Z0AA"))

test_df2b <- readr::read_csv(build_url("0208020Z0BB"))

test_df3 <- readr::read_csv(build_url("0208020Z0AAAAA"))