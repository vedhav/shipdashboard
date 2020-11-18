#' To filter the ship from the ships_data and return the basic ship details
#'
#' @param ships_data A dataframe containing the ships data required by this app
#' @param ship_name A string containing the ship name whos details has to be extracted
#'
#' @return A list with the details about the ship
get_ship_details <- function(ships_data, ship_name) {
    must_have_columns <- c("SHIP_ID", "SHIPNAME", "ship_type", "LENGTH", "WIDTH", "DWT", "FLAG")
    if (!all(must_have_columns %in% names(ships_data))) {
        stop(
            paste0(
                "These columns are missing from your data: ",
                paste(must_have_columns[!must_have_columns %in% names(ships_data)], collapse = ", ")
            )
        )
    }
    this_ship <- ships_data %>% dplyr::filter(SHIPNAME == ship_name) %>% head(1)
    if (nrow(this_ship) == 0) {
        stop(paste0("There is no ship named `", ship_name, "`"))
    }
    ship_details <- list(
        ship_id = as.numeric(this_ship$SHIP_ID),
        ship_name = this_ship$SHIPNAME,
        ship_type = this_ship$ship_type,
        length = this_ship$LENGTH,
        width = this_ship$WIDTH,
        dead_weight = this_ship$DWT,
        flag = this_ship$FLAG
    )
    return(ship_details)
}

#' To format the ship type to get the ship image file
#'
#' @param ship_type A string with the ship's type
#' @param type A string which contains the suffix that can be appended to the file name. Default is empty string
#' @param file_extension A string which contains file extension. Defaults to .png
#'
#' @return A string which contains the file name corresponding to the ship type
#'
#' @examples
#' format_ship_type("Cargo")
format_ship_type <- function(ship_type, suffix = "", file_extension = "png") {
    if (!ship_type %in% ship_types_enum) {
        stop(paste0("The ship_type must contain any onne of these: ", paste(ship_types_enum, collapse = ", ")))
    }
    file_name <- ship_type %>% tolower %>% stringr::str_replace_all(" +", "_") %>%
        paste0(suffix, ".", file_extension)
    return(file_name)
}

#' To filter the current ship's data from the whole dataset
#'
#' @param ship_id A number with the ship's id
#'
#' @return A tibble with only this ship's data
get_current_ship_data <- function(ship_id) {
    get_data_from_db(
        "SELECT * FROM ais_data WHERE SHIP_ID = ?",
        list(ship_id)
    ) %>% dplyr::arrange(dplyr::desc(DATETIME))
}

#' To filter the data with the longest distance measure between two consecutive AIS readings
#'
#' @param current_ship_data A tbl_df/tbl/data.frame with a particular ship's AIS data
#' @param geo_measure An optional string argument to set accuracy for the geo distance calculation.
#' Can be "haversine" "vincenty", "geodesic", or "cheap"
#' @param round_accu An integer to specify the rounding digits accuracy
#'
#' @return A tibble with two consecutive rows of AIS data with the most distance travelled
get_longest_ais_gap_data <- function(current_ship_data, geo_measure = "geodesic", round_accu = 0) {
    longest_ais_gap_data <- current_ship_data %>%
        dplyr::mutate(distance = round(c(geodist::geodist(., sequential = TRUE, measure = geo_measure), round_accu))) %>%
        dplyr::arrange(dplyr::desc(distance), dplyr::desc(DATETIME)) %>% head(2)
    longest_ais_gap_data$position <- c("Stop", "Start")
    return(longest_ais_gap_data)
}
