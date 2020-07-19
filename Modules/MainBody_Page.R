ui_mainBody_Page <- function(id) {
  ns <- NS(id)
  tagList(
    radioGroupButtons(ns("ordinary_Proportion"), choices = c("Absolute counts", "Relative counts"), selected = "Absolute counts", justified = TRUE, status = "primary"),
    conditionalPanel("input.ordinary_Proportion == 'Absolute counts'",
      ns = ns,
      ui_mainBody(ns("uiMain_ord_country"))
    ),
    conditionalPanel("input.ordinary_Proportion == 'Relative counts'",
      ns = ns,
      ui_mainBody(ns("uiMain_prop_country"), FALSE)
    )
  )
}
server_mainBody_Page <- function(input, output, session, data) {
  data2 <- compute_cum_ranks(data)
  data_prop <- compute_proportion(data, pop = data$Population, n = 1e6)
  data_prop <- compute_cum_ranks(data_prop)

  callModule(server_mainBody, "uiMain_ord_country", data = data2)
  callModule(server_mainBody, "uiMain_prop_country", data_prop)
}
