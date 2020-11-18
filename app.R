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
source("R/ship_path_module.R")

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
                            ship_path_ui("main_ship_path")
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
        ship_ais_data <- callModule(module = ship_path, id = "main_ship_path", ship_details = this_ship_details)
        if (nrow(ship_ais_data) == 0) {
            toast("There was no data avaliable for this ship.")
            return()
        }
        longest_ship_ais_data <- get_longest_ais_gap_data(ship_ais_data)
        ship_distance <- longest_ship_ais_data$distance[1]
        time_difference <- round(difftime(longest_ship_ais_data$DATETIME[2], longest_ship_ais_data$DATETIME[1]), 2) %>% abs
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
                    tags$b("Deadweight Tonnage: "), this_ship_details$dead_weight, " tons", tags$br()
                )
            )
        })
        output$ship_position_desc <- renderText({
            paste0(
                this_ship_details$ship_name, " sailed the longest distance between two consecutive observations of ",
                ship_distance, "m on ", longest_ship_ais_data$DATETIME[2], " for ", time_difference, " ", units(time_difference)
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
                    lng = ~LON, lat = ~LAT,
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
    })
}

# Running the shiny app
shinyApp(ui, server)