# UI of the ship dropdowns

ship_inputs_ui <- function(id) {
    ns <- NS(id)
    div(
        dropdown_input(input_id = ns("ship_type"), choices = sort(ship_types_enum), value = ship_types_enum[1]), br(),
        dropdown_input(input_id = ns("ship_name"), choices = NULL)
    )
}

# Server logic for the ship input module
ship_inputs <- function(input, output, session, data) {
    observe({
        req(input$ship_type)
        update_dropdown_input(session, input_id = "ship_name", choices = data %>% filter(ship_type == input$ship_type) %>% pull(SHIPNAME) %>% unique)
    })
    current_ship_details <- reactive({
        req(input$ship_name)
        get_ship_details(data, input$ship_name)
    })
}
