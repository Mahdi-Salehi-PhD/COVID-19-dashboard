ui_leafMap_Page <- function(id) {
  ns <- NS(id)
  tagList(
    radioGroupButtons(ns("O_P"), choices = c("Absolute counts", "Relative counts"), selected = "Absolute counts", justified = TRUE, status = "primary"),
    conditionalPanel("input.O_P == 'Absolute counts'",
      ns = ns,
      tabsetPanel(
        tabPanel(
          "Confirmed",
          ui_leafMap(ns("map_case_ord_country"))
        ),
        tabPanel(
          "Death",
          ui_leafMap(ns("map_death_ord_country"))
        ),
        tabPanel(
          "Recovered",
          ui_leafMap(ns("map_recovered_ord_country"))
        )
      )
    ),
    conditionalPanel("input.O_P == 'Relative counts'",
      ns = ns,
      tabsetPanel(
        tabPanel(
          "Confirmed",
          ui_leafMap(ns("map_case_prop_country"))
        ),
        tabPanel(
          "Death",
          ui_leafMap(ns("map_death_prop_country"))
        )
      )
    )
  )
}

server_leafMap_Page <- function(input, output, session, data) {
  data2 <- compute_cum_ranks(data)

  data_prop <- compute_proportion(data, pop = data$Population, n = 1e6)
  data_prop <- compute_cum_ranks(data_prop)
  callModule(server_leafMap, "map_case_ord_country", data2, var2show = "case")
  callModule(server_leafMap, "map_death_ord_country", data2, var2show = "death")
  callModule(server_leafMap, "map_recovered_ord_country", data2, var2show = "recovered")

  callModule(server_leafMap, "map_case_prop_country", data_prop, var2show = "case")
  callModule(server_leafMap, "map_death_prop_country", data_prop, var2show = "death")
}

ui <- fluidPage(
  ui_leafMap_Page("f_1")
)
server <- function(input, output) {
  data <- DataDownload()
  callModule(server_leafMap_Page, "f_1", data)
}
shinyApp(ui, server)
