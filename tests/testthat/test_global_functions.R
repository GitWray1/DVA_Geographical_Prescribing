
source("R/global_functions.R")
library(testthat)

# All unit tests are made up of an expect_ function
# The context("describe context") line allows us to group tests with the 
# same “context” - a description.

# tidy_date tests ----------------------------------------------------------

context("Value checks")
test_that("tidy_date() returns expected values", {
    test1 <- "2021-06-01" # character format
    test2 <- lubridate::as_date("2021-06-01") # date format
    test3 <- "2020-02-29" # leap year
    expect_equal(tidy_date(test1), "Jun 2021")
    expect_equal(tidy_date(test2), "Jun 2021")
    expect_equal(tidy_date(test3), "Feb 2020")
})

context("Class checks")
test_that("tidy_date() returns expected class", {
    test1 <- "2021-06-01" # character format
    test2 <- lubridate::as_date("2021-06-01") # date format
    expect_match(class(tidy_date(test1)), "character")
    expect_match(class(tidy_date(test2)), "character")
})

context("Error checks")
test_that("tidy_date() deals with errors correctly", {
    test1 <- "2021-02-29" # incorrect leap year
    test2 <- "2020-02" # wrong format
    test3 <- 2020-06-01 # wrong class
    expect_error(tidy_date(test1), 
                 "Check date is valid and in format 'yyyy-mm-dd'",
                 fixed = TRUE)
    expect_error(tidy_date(test2), 
                 "Check date is valid and in format 'yyyy-mm-dd'",
                 fixed = TRUE)
    expect_error(tidy_date(test3), 
                 "Input must be either in character or date format",
                 fixed = TRUE)
})


# tidy number tests -------------------------------------------------------

context("Value checks")
test_that("tidy_number() returns expected values", {
    test1 <- 2000L # integer format
    test2 <- 10000.548 # double format
    test3 <- 1500.00 # trailing 0s
    expect_equal(tidy_number(test1), "2,000")
    expect_equal(tidy_number(test2), "10,000.55")
    expect_equal(tidy_number(test3), "1,500")
})

context("Class checks")
test_that("tidy_number() returns expected class", {
    test1 <- 2000L # integer format
    test2 <- 10000.548 # double format
    expect_equal(class(tidy_number(test1)), "character")
    expect_equal(class(tidy_number(test2)), "character")
})

context("Error checks")
test_that("tidy_number() deals with errors correctly", {
    test1 <- c() # wrong length
    expect_error(tidy_number(test1), 
                 "Input vector must of non-zero length",
                 fixed = TRUE)
})


# get_tidy_area tests -----------------------------------------------------

context("Value checks")
test_that("get_tidy_area() returns expected values", {
    test1 <- "ccg"
    test2 <- "stp"
    test3 <- "region"
    test4 <- "ics"
    expect_equal(get_tidy_area(test1), "CCG")
    expect_equal(get_tidy_area(test2), "STP")
    expect_equal(get_tidy_area(test3), "Region")
    expect_equal(get_tidy_area(test4), "Unknown area input")
    expect_equal(class(get_tidy_area(test1)), "character")
})

context("class checks")
test_that("get_tidy_area() returns expected class", {
    test1 <- "ccg"
    expect_equal(class(get_tidy_area(test1)), "character")
})

context("Error checks")
test_that("get_tidy_area() deals with errors correctly", {
    test1 <- character() # empty vector
    expect_error(get_tidy_area(test1), 
                 "Input vector must be non-zero length",
                 fixed = TRUE)
})
