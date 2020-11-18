library(tidyverse)
library(testthat)
source("../../R/app_constants.R")
context("Checking data related functions")

ships_data <- readRDS("../../data/ships.RDS")

test_that("get_ship_details works", {
    vega_details <- get_ship_details(ships_data, "VEGA")
    expect_type(vega_details, "list")
    expect_length(vega_details, 7)
    expect_equal(vega_details$ship_name, "VEGA")
    expect_error(get_ship_details("random ship name"))
})

test_that("get_longest_ais_gap_data works", {
    vega_details <- get_ship_details(ships_data, "VEGA")
    vega_data <- ships_data %>% filter(SHIP_ID == vega_details$ship_id)
    vega_longest_ais_gap <- get_longest_ais_gap_data(vega_data)
    expect_equal(nrow(vega_longest_ais_gap), 2)
    expect_equal(length(unique(vega_longest_ais_gap$SHIP_ID)), 1)
    expect_true(all(c("distance", "DATETIME") %in% names(vega_longest_ais_gap)))
})
