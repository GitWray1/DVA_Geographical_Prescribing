
source("R/filtering_functions.R")
library(dplyr)
library(testthat)

# Filter_for_line ---------------------------------------------------------

test_df <- readr::read_csv("data/output_files/DOACs_data_long.csv")

# Example line output 1
line_out_df1 <- test_df %>% 
    filter(chemical == "Apixaban",
           area_type == "ccg") %>%
    mutate(items_per_1000 = (items/(registered_patients/1000)),
           quantity_per_1000 = (quantity/(registered_patients/1000)),
           actual_cost_per_1000 = (actual_cost/(registered_patients/1000)))

# Example line output 2
line_out_df2 <- test_df %>% 
    filter(chemical == "Warfarin sodium",
           area_type == "region") %>%
    mutate(items_per_1000 = (items/(registered_patients/1000)),
           quantity_per_1000 = (quantity/(registered_patients/1000)),
           actual_cost_per_1000 = (actual_cost/(registered_patients/1000)))


context("filter_for_line value checks")
test_that("filter_for_line() returns expected values", {
    test1 <- filter_for_line(test_df, "Apixaban", "ccg")
    test2 <- filter_for_line(test_df, "Warfarin sodium", "region")
    
    expect_equal(length(test1), 14)
    expect_equal(length(test2), 14)
    expect_equal(test1, line_out_df1)
    expect_equal(test2, line_out_df2)
})


context("filter_for_line class checks")
test_that("filter_for_line() returns expected class", {
    test1 <- filter_for_line(test_df, "Apixaban", "ccg")
    expect_equal(typeof(test1), "list")
    expect_equal(class(test1), c("tbl_df", "tbl","data.frame"))
})


context("filter_for_line error checks")
test_that("filter_for_line() deals with errors correctly", {
    test1 <- test_df %>%
                rename(medicine = chemical)
    expect_error(filter_for_line(test1 ,"Apixaban", "ccg"),
                 "Dataframe must contain correct column names",
                 fixed = TRUE)
    expect_error(filter_for_line(test_df, "test_medicine", "ccg"), 
                 "Dataframe does not contain selected medicine",
                 fixed = TRUE)
    expect_error(filter_for_line(test_df, "Warfarin sodium", "test_region"), 
                 "Dataframe does not contain selected area type",
                 fixed = TRUE)
})


# Filter for bar tests ----------------------------------------------------

# Example bar output 1
bar_out_df1 <- line_out_df1 %>% 
            filter(date >= "2020-01-01",
                   date <= "2020-12-31") %>% 
            group_by(chemical, bnf_code, name, ods_code, gss_code) %>% 
            summarise(registered_patients = sum(registered_patients),
                      items = sum(items), 
                      quantity = sum(quantity), 
                      actual_cost = sum(actual_cost)) %>% 
            ungroup() %>% 
            mutate(items_per_1000 = (items/(registered_patients/1000)),
                   quantity_per_1000 = (quantity/(registered_patients/1000)),
                   actual_cost_per_1000 = (actual_cost/(registered_patients/1000))) %>% 
            select(-registered_patients)

# Example bar output 2
bar_out_df2 <- line_out_df2 %>% 
            filter(date >= "2021-01-01",
                   date <= "2021-06-30") %>% 
            group_by(chemical, bnf_code, name, ods_code, gss_code) %>% 
            summarise(registered_patients = sum(registered_patients),
                      items = sum(items), 
                      quantity = sum(quantity), 
                      actual_cost = sum(actual_cost)) %>% 
            ungroup() %>% 
            mutate(items_per_1000 = (items/(registered_patients/1000)),
                   quantity_per_1000 = (quantity/(registered_patients/1000)),
                   actual_cost_per_1000 = (actual_cost/(registered_patients/1000))) %>% 
            select(-registered_patients)

context("filter_for_bar value checks")
test_that("filter_for_bar() returns expected values", {
    test1 <- filter_for_bar(line_out_df1, c("2020-01-01", "2020-12-31"))
    test2 <- filter_for_bar(line_out_df2, c("2021-01-01", "2021-06-30"))
    expect_equal(length(test1), 11)
    expect_equal(length(test2), 11)
    expect_equal(test1, bar_out_df1)
    expect_equal(test2, bar_out_df2)
})

context("filter_for_bar class checks")
test_that("filter_for_bar() returns expected class", {
    test1 <- filter_for_bar(line_out_df1, c("2020-01-01", "2020-12-31"))
    expect_equal(typeof(test1), "list")
    expect_equal(class(test1), c("tbl_df", "tbl","data.frame"))
})

context("filter_for_bar error checks")
test_that("filter_for_bar() deals with errors correctly", {
    test1 <- line_out_df1 %>%
                rename(medicine = chemical)
    expect_error(filter_for_bar(test1, c("2020-01-01", "2020-12-31")),
                 "Dataframe must contain correct column names",
                 fixed = TRUE)
})

# Filter for map tests ----------------------------------------------------

# Example map output 1
map_out_df1 <- shp_files %>% inner_join(bar_out_df1)

# Example map output 2
map_out_df2 <- shp_files %>% inner_join(bar_out_df2)

context("filter_for_map value checks")
test_that("filter_for_map() returns expected values", {
    test1 <- filter_for_map(bar_out_df1, shp_files)
    test2 <- filter_for_map(bar_out_df2, shp_files)
    expect_equal(nrow(test1), 106)
    expect_equal(nrow(test2), 7)
    expect_equal(test1, map_out_df1)
    expect_equal(test2, map_out_df2)
})

context("filter_for_map class checks")
test_that("filter_for_map() returns expected class", {
    test1 <- filter_for_map(bar_out_df1, shp_files)
    test2 <- filter_for_map(bar_out_df2, shp_files)
    expect_equal(typeof(test1), "list")
    expect_equal(class(test1), c( "sf", "tbl_df", "tbl", "data.frame"))
    expect_equal(typeof(test2), "list")
    expect_equal(class(test2), c( "sf", "tbl_df", "tbl", "data.frame"))
})

context("filter_for_map error checks")
test_that("filter_for_map() deals with errors correctly", {
    shp_test <- shp_files %>%
                    sf::st_drop_geometry()
    test1 <- bar_out_df1 %>%
                select(-c("gss_code", "name"))
    expect_error(filter_for_map(bar_out_df1, shp_test),
                 "shape_files_df must have a geometry column (sf)",
                 fixed = TRUE)
    expect_error(filter_for_map(test1, shp_files),
                 "bar_df and shape_files_df must have a shared column",
                 fixed = TRUE)
})
