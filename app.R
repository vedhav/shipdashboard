# Loading packages
library(shiny)
library(shiny.semantic)
library(tidyverse)
library(leaflet)
library(plotly)
library(dbx)
library(RMySQL)
library(geodist)

# Sourcing the required R constants, functions and modules
source("R/app_constants.R")
source("R/database_functions.R")
source("R/business_logic_functions.R")
source("R/ship_dropdown_module.R")

# Loading the data
ships_data <- get_data_from_db("SELECT * FROM `ships_data`") %>% arrange(SHIPNAME)

# UI of the shiny app
ui <- semanticPage(
    title = "Ship dashboard",
    tags$head(tags$link(rel = "shortcut icon", type = "image/png", href = "high_special.png")),
    div(
        class = "ui container grid", style = "margin-top: 5px",
        div(
            class = "row centered", style = "color: #8a8a8a; font-size: 2rem",
            "Ship dashboard"
        ),
        div(
            class = "four wide column",
            ship_inputs_ui("main_ship_input"), br(),
            uiOutput("info_ui")
        ),
        div(
            class = "twelve wide column",
            tabset(
                list(
                    list(
                        menu = div("Ship position"),
                        content = div(
                            div(class = "row centered", segment(textOutput("ship_position_desc"))),
                            leafletOutput("ship_position_map"),
                            "* This might not be the actual distance travelled by the ship"
                        )
                    ),
                    list(
                        menu = div("Ship path"),
                        content = div(
                            div(
                                class = "ui raised segment grid",
                                div(
                                    class = "one wide column",
                                    icon("clock")
                                ),
                                div(
                                    class = "five wide column",
                                    uiOutput("ship_pointer_date")
                                ),
                                div(
                                    class = "five wide column",
                                    uiOutput("ship_pointer_time")
                                ),
                                div(
                                    class = "five wide column",
                                    uiOutput("ship_pointer_speed")
                                )
                            ),
                            leafletOutput("ship_path_map", height = "300px"),
                            uiOutput("ship_pointer_slider_ui", style = "margin-left: 50px;"),
                            plotlyOutput("ship_time_speed_plot", width = "100%", height = "300px")
                        )
                    )
                )
            )
        )
    )
)

# Server logic of the shiny app
server <- function(input, output, session) {
    selected_ship_inputs <- callModule(module = ship_inputs, id = "main_ship_input", data = ships_data)
    observe({
        this_ship_details <- selected_ship_inputs()
        ship_ais_data <- get_current_ship_data(this_ship_details$ship_id)
        if (nrow(ship_ais_data) < 2) {
            return()
        }
        longest_ship_ais_data <- get_longest_ais_gap_data(ship_ais_data)
        ship_distance <- longest_ship_ais_data$distance[1]
        time_difference <- round(difftime(longest_ship_ais_data$DATETIME[2], longest_ship_ais_data$DATETIME[1]), 2) %>% abs
        output$ship_pointer_slider_ui <- renderUI({
            slider_input("ship_path_pointer", 1, 1, nrow(ship_ais_data))
        })
        output$ship_time_speed_plot <- renderPlotly({
            plot_ly(
                ship_ais_data %>% arrange(DATETIME),
                x = ~DATETIME, y = ~SPEED, line = list(color = "#8a8a8a"),
                type = "scatter", mode = "lines"
            ) %>%
            layout(xaxis = list(title = "Date time", showgrid = FALSE), yaxis = list(title = "Speed (knots)", showgrid = FALSE)) %>%
            config(displayModeBar = FALSE)
        })
        output$info_ui <- renderUI({
            div(
                class = "ui card",
                div(
                    class = "content",
                    div(class = "right floated meta", this_ship_details$ship_type),
                    img(
                        class = "ui avatar image",
                        src = format_ship_type(this_ship_details$ship_type, "_bw")
                    ),
                    tags$b(this_ship_details$ship_name)
                ),
                div(
                    class = "content",
                    tags$b("Ship ID: "), this_ship_details$ship_id, tags$br(),
                    tags$b("Flag: "), this_ship_details$flag, tags$br(),
                    tags$b("Length: "), this_ship_details$length, " m", tags$br(),
                    tags$b("Width: "), this_ship_details$width, " m", tags$br(),
                    tags$b("Deadweight Tonnage: "), this_ship_details$dead_weight, " tons", tags$br(),
                    tags$b("Average Speed: "), round(mean(ship_ais_data$SPEED)), " knots", tags$br(),
                    tags$b("Latest Status: "), ifelse(ship_ais_data$is_parked[1], "Parked at ", "Sailing towards "),
                    ship_ais_data$DESTINATION[1]
                )
            )
        })
        output$ship_position_map <- renderLeaflet({
            if (nrow(longest_ship_ais_data) == 0) {
                toast("There was no data for this particular ship")
                return()
            }
            leaflet(longest_ship_ais_data, options = leafletOptions(attributionControl = FALSE)) %>%
                addProviderTiles("CartoDB.PositronNoLabels") %>%
                addMarkers(
                    icon = ~makeIcon(
                        format_ship_type(this_ship_details$ship_type, ifelse(position == "Start", "_t", "")),
                        iconWidth = 50, iconAnchorX = 20, iconAnchorY = 25
                    ),
                    popup = ~paste0(
                        position, " coordinates", br(),
                        "Latitude: ", LAT, "<br>",
                        "Longitude: ", LON, "<br>",
                        "Time: ", format(DATETIME, "%b %d, %Y %H:%M:%S")
                    )
                ) %>%
                addControl(
                    html = paste0(
                        "<img src='", format_ship_type(this_ship_details$ship_type, "_t"),
                        "' style='height:15px;'> ", this_ship_details$ship_type, " started<br/>
                        <img src='", format_ship_type(this_ship_details$ship_type),
                        "' style='height:15px;'> ", this_ship_details$ship_type, " stopped"
                    ),
                    position = "topright"
                ) %>%
                addPolylines(
                    lng = ~LON, lat = ~LAT, weight = 2, color = "#7f7f7f",
                    label = HTML(
                        paste0("*Distance: ", ship_distance , " m<br>Time: ", time_difference, " ", units(time_difference))
                    ),
                    labelOptions = labelOptions(permanent = TRUE)
                )
        })
        output$ship_position_desc <- renderText({
            paste0(
                this_ship_details$ship_name, " sailed the longest distance between two consecutive observations of ",
                ship_distance, "m on ", longest_ship_ais_data$DATETIME[2], " for ", time_difference, " ", units(time_difference)
            )
        })
        output$ship_path_map <- renderLeaflet({
            if (nrow(ship_ais_data) == 0) {
                toast("There was no data for this particular ship")
                return()
            }
            leaflet(ship_ais_data, options = leafletOptions(attributionControl = FALSE)) %>%
                addProviderTiles("CartoDB.PositronNoLabels") %>%
                addPolylines(lng = ~LON, lat = ~LAT, weight = 2, color = "#7f7f7f",)
        })
        observeEvent(input$ship_path_pointer, {
            ship_pointer_data <- ship_ais_data[nrow(ship_ais_data) - input$ship_path_pointer + 1,]
            leafletProxy("ship_path_map", data = ship_pointer_data) %>%
                clearMarkers() %>%
                addMarkers(
                    icon = makeIcon(
                        format_ship_type(this_ship_details$ship_type),
                        iconWidth = 50, iconAnchorX = 20, iconAnchorY = 25
                    )
                )
            output$ship_pointer_date <- renderUI({
                HTML(paste0(tags$b("Date: "), ship_pointer_data$DATETIME %>% format("%d-%m-%Y")))
            })
            output$ship_pointer_time <- renderUI({
                HTML(paste0(tags$b("Time: "), ship_pointer_data$DATETIME %>% format("%H:%M:%S")))
            })
            output$ship_pointer_speed <- renderUI({
                HTML(paste0(tags$b("Speed: "), ship_pointer_data$SPEED, " knots"))
            })
        })
    })
}

# Running the shiny app
shinyApp(ui, server)