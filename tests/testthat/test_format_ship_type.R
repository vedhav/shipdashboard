library(testthat)
source("../../R/app_constants.R")
context("Checking format_ship_type")

test_that("format_ship_type works", {
    expect_equal(format_ship_type("Cargo"), "cargo.png")
    expect_equal(format_ship_type("Cargo", "_bw"), "cargo_bw.png")
    expect_equal(format_ship_type("Cargo", "_t", "jpg"), "cargo_t.jpg")
    expect_error(format_ship_type("random ship type"))
})

test_that("All the image files from the ship_types_enum is present inside www", {
    base_files_needed <- lapply(ship_types_enum, FUN = format_ship_type) %>% unlist
    transparent_files_needed <- lapply(ship_types_enum, FUN = format_ship_type, suffix = "_t") %>% unlist
    black_white_files_needed <- lapply(ship_types_enum, FUN = format_ship_type, suffix = "_bw") %>% unlist
    expect_true(all(file.exists(paste0("../../www/", c(base_files_needed, transparent_files_needed, black_white_files_needed)))))
})
