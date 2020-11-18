# UI of the ship path
ship_path_ui <- function(id) {
    ns <- NS(id)
    div(
        div(
            class = "ui raised segment grid",
            div(
                class = "one wide column",
                icon("clock")
            ),
            div(
                class = "five wide column",
                uiOutput(ns("ship_pointer_date"))
            ),
            div(
                class = "five wide column",
                uiOutput(ns("ship_pointer_time"))
            ),
            div(
                class = "five wide column",
                uiOutput(ns("ship_pointer_speed"))
            )
        ),
        leaflet::leafletOutput(ns("ship_path_map"), height = "300px"),
        uiOutput(ns("ship_pointer_slider_ui"), style = "margin-left: 50px;"),
        plotly::plotlyOutput(ns("ship_time_speed_plot"), width = "100%", height = "300px")
    )
}

# Server logic for the ship path module
ship_path <- function(input, output, session, ship_details) {
    ns <- session$ns
    ship_ais_data <- get_current_ship_data(ais_data, ship_details$ship_id)
    if (nrow(ship_ais_data) < 2) {
        return(tidyr::tibble())
    }
    output$ship_pointer_slider_ui <- renderUI({
        shiny.semantic::slider_input(ns("ship_path_pointer"), 1, 1, nrow(ship_ais_data))
    })
    output$ship_time_speed_plot <- renderPlotly({
        plotly::plot_ly(
            ship_ais_data %>% dplyr::arrange(DATETIME),
            x = ~DATETIME, y = ~SPEED, line = list(color = "#8a8a8a"),
            type = "scatter", mode = "lines"
        ) %>%
        plotly::layout(xaxis = list(title = "Date time", showgrid = FALSE), yaxis = list(title = "Speed (knots)", showgrid = FALSE)) %>%
        config(displayModeBar = FALSE)
    })
    output$ship_path_map <- renderLeaflet({
        if (nrow(ship_ais_data) == 0) {
            shiny.semantic::toast("There was no data for this particular ship")
            return()
        }
        leaflet::leaflet(ship_ais_data, options = leaflet::leafletOptions(attributionControl = FALSE)) %>%
            leaflet::addProviderTiles("CartoDB.PositronNoLabels") %>%
            leaflet::addPolylines(lng = ~LON, lat = ~LAT, weight = 2, color = "#7f7f7f",)
    })
    observeEvent(input$ship_path_pointer, {
        ship_pointer_data <- ship_ais_data[nrow(ship_ais_data) - input$ship_path_pointer + 1,]
        leaflet::leafletProxy("ship_path_map", data = ship_pointer_data) %>%
            leaflet::clearMarkers() %>%
            leaflet::addMarkers(
                lng = ~LON, lat = ~LAT,
                icon = leaflet::makeIcon(
                    format_ship_type(ship_details$ship_type),
                    iconWidth = 50, iconAnchorX = 20, iconAnchorY = 25
                )
            )
        output$ship_pointer_date <- renderUI({
            htmltools::HTML(paste0(tags$b("Date: "), ship_pointer_data$DATETIME %>% format("%d-%m-%Y")))
        })
        output$ship_pointer_time <- renderUI({
            htmltools::HTML(paste0(tags$b("Time: "), ship_pointer_data$DATETIME %>% format("%H:%M:%S")))
        })
        output$ship_pointer_speed <- renderUI({
            htmltools::HTML(paste0(tags$b("Speed: "), ship_pointer_data$SPEED, " knots"))
        })
    })
    return(ship_ais_data)
}
