library(shiny)
ui_moran_page <- function(id) {
  ns <- NS(id)
  tagList(
    radioGroupButtons(ns("rdio"), choiceNames = c("Absolute Counts", "Relative Counts"), choiceValues = c("absCount", "relCount"), status = "primary", justified = TRUE),
    conditionalPanel("input.rdio=='absCount'",
      ns = ns,
      ui_moran(ns("moran1"))
    ),
    conditionalPanel("input.rdio=='relCount'",
      ns = ns,
      ui_moran(ns("moran2"))
    )
  )
}

server_moran_page <- function(input, output, session, data) {
  data2 <- compute_proportion(data, data$Population)
  callModule(server_moran, "moran1", data)
  callModule(server_moran, "moran2", data2)
}
