library(testthat)
source("../../R/app_constants.R")
context("Checking get_ship_details")

ships_data <- readRDS("../../data/ships.RDS")

test_that("get_ship_details works", {
    vega_details <- get_ship_details(ships_data, "VEGA")
    expect_type(vega_details, "list")
    expect_length(vega_details, 7)
    expect_equal(vega_details$ship_name, "VEGA")
})
