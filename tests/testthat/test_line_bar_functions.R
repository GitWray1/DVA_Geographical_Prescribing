
library(testthat)

source("R/line_bar_functions.R")


# Filter_for_line ---------------------------------------------------------

context("filter_for_line value checks")
test_that("filter_for_line() returns expected values", {
    out_names <- c("date", "chemical", "bnf_code", "area_type", "name", "ods_code", "gss_code", 
                   "registered_patients", "items", "quantity", "actual_cost", "items_per_1000", 
                   "quantity_per_1000", "actual_cost_per_1000")
    expect_equal(typeof(filter_for_line(df, "Apixaban", "ccg")), "list")
    expect_equal(class(filter_for_line(df, "Apixaban", "ccg")), c("tbl_df", "tbl","data.frame"))
    expect_equal(length(filter_for_line(df, "Apixaban", "ccg")), 14)
    expect_equal(names(filter_for_line(df, "Apixaban", "ccg")), out_names)
})



temp <- df %>% 
    filter(chemical == "Apixaban",
           area_type == "ccg") %>%
    mutate(items_per_1000 = (items/(registered_patients/1000)),
           quantity_per_1000 = (quantity/(registered_patients/1000)),
           actual_cost_per_1000 = (actual_cost/(registered_patients/1000)))

test <- names(temp)
test
