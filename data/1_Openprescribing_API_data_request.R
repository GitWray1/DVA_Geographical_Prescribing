# Load Packages -----------------------------------------------------------

library(dplyr)

# BNF code lookup - readr::read_csv("1_data_prep/input_files/BNF_code_lookup.csv")

# Functions ---------------------------------------------------------------

# Function to create API url when given a bnf code
build_url <- function(url_bnf_code){
  url_base <- "https://openprescribing.net/api/1.0/spending_by_ccg/?code="
  url_end <- "&format=csv"
  csv_url <- paste0(url_base,
                    url_bnf_code,
                    url_end)
  return(csv_url)
}

# Function to extract csv from async response
extract_csv <- function(x){
  tmp_response <- x$parse("UTF-8")
  response_df <- readr::read_csv(tmp_response)
  return(response_df)
}


# Import area and population data -----------------------------------------

# Lookups to map CCGs to STPs and regions, population of each CCG by month
STP_to_region_lookup <- readr::read_csv("1_data_prep/input_files/STP_to_region_(April_2021)_lookup.csv")
CCG_to_STP_lookup <- readr::read_csv("1_data_prep/input_files/CCG_to_STPs_(April_2021)_lookup.csv")
CCG_pop_df <- readr::read_csv("https://openprescribing.net/api/1.0/org_details/?org_type=ccg&keys=total_list_size&format=csv")

# Join area lookups into one, drop unused column and clean column names
areas_lookup <- left_join(STP_to_region_lookup, CCG_to_STP_lookup) %>% 
  select(-FID) %>% 
  janitor::clean_names()

# Remove names (we have this elsewhere), rename code column for upcoming join
CCG_pop_df <- CCG_pop_df %>% 
  select(-row_name) %>% 
  rename("ccg21cdh" = row_id)

# Join lookup data to population dataset
area_pop_df <- left_join(CCG_pop_df, areas_lookup) %>% 
  rename("registered_patients" = total_list_size) %>% 
  select(date, 
         nhser21nm, nhser21cdh, nhser21cd,
         stp21nm, stp21cdh, stp21cd,
         ccg21nm, ccg21cdh, ccg21cd,
         registered_patients)

# Import list of chemicals and codes  -------------------------------------

chemical_bnf_codes <- readr::read_csv("1_data_prep/input_files/DOAC_codes.csv",
                                              col_types = readr::cols(chemical = readr::col_character(),
                                                                      bnf_code = readr::col_character()))


# Create URLs for API -----------------------------------------------------

chemical_bnf_codes <- chemical_bnf_codes %>% 
  mutate("csv_url" = purrr::map_chr(bnf_code, build_url)) %>% 
  filter(!bnf_code %in% c("0407020D0","0407020Z0","0407020U0"))

# Removed the following as they return no data (no prescribing) and throw errors
# Dextromoramide tartrate, Oxycodone, Pentazocine lactate


# Create API request ------------------------------------------------------

# Create asynchronous API object and get results
dd <- crul::Async$new(urls = chemical_bnf_codes$csv_url)
res <- dd$get()

# Check that everything is a success
all(vapply(res, function(z) z$success(), logical(1)))

# Extract csv files from async responses
async_dfs <- res %>% purrr::map(extract_csv)


# Clean up data -----------------------------------------------------------

# Add chemical name and bnf_code into each df and clean up a bit
for (i in seq_along(async_dfs)){
  async_dfs[[i]] <- async_dfs[[i]] %>% 
    mutate("chemical" = chemical_bnf_codes$chemical[[i]],
           "bnf_code" = as.character(chemical_bnf_codes$bnf_code[[i]])) %>% 
    relocate(date, chemical, bnf_code, row_id, 
           row_name, items, quantity, actual_cost)
}

# Concatenate the results and filter out NHS Vale Royal - old CCG that has since closed
final_df <- bind_rows(async_dfs) %>% 
                filter(!row_id == "02D")

# Join to population and area dataset, drop duplicate column, rename and reorder columns
final_df <- final_df %>% 
                left_join(area_pop_df, by = c("date" = "date", 
                                              "row_id" = "ccg21cdh")) %>% 
                select(-row_name) %>% 
                rename("ccg_ods" = row_id,
                       "ccg_gss" = ccg21cd,
                       "ccg_name" = ccg21nm,
                       "stp_ods" = stp21cdh,
                       "stp_gss" = stp21cd,
                       "stp_name" = stp21nm,
                       "region_ods" = nhser21cdh,
                       "region_gss" = nhser21cd,
                       "region_name" = nhser21nm) %>% 
                relocate(ccg_ods, ccg_gss, registered_patients, 
                         items, quantity, actual_cost, .after = ccg_name)

# Write data to csv file
readr::write_csv(final_df, "1_data_prep/output_files/DOACs_data.csv")
readr::write_csv(area_pop_df, "1_data_prep/output_files/area_populations_df.csv")


# Also create long data table ---------------------------------------------

area_types = c("ccg", "stp", "region")

long_df <- tibble()

for (i in seq_along(area_types)){
    
    temp <- final_df %>% 
                group_by(date, chemical, bnf_code, across(contains(area_types[[i]]))) %>% 
                summarise("registered_patients" = sum(registered_patients), 
                          "items" = sum(items),
                          "quantity" = sum(quantity),
                          "actual_cost" = sum(actual_cost)) %>% 
                ungroup() %>%
                rename("name" = paste0(area_types[[i]], "_name"),
                       "ods_code" = paste0(area_types[[i]], "_ods"),
                       "gss_code" = paste0(area_types[[i]], "_gss")) %>% 
                mutate(area_type = area_types[[i]], .before = name)
    
    long_df <- rbind(long_df, temp)
}

readr::write_csv(long_df, "1_data_prep/output_files/DOACs_data_long.csv")
