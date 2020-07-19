library(shiny)
library(plotly)
library(dplyr)
f_labels <- function(x) {
  switch(x,
    "Cases" = return("New cases"),
    "cum_cases" = return("Cumulative confirmed cases"),
    "Deaths" = return("New deaths"),
    "cum_death" = return("Cumulative deaths"),
    "Recovered" = return("New recovereis"),
    "cum_recovered" = return("Cumulative recoveries")
  )
}
ui_barCharts <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("inp"), label = "Day", min = 1, max = 10, value = 10, step = 2, width = "400px", animate = TRUE),
    plotlyOutput(ns("plt"))
  )
}
server_barCharts <- function(input, output, session, data, Var2show) {

  # sliderLabel <- reactiveVal()
  # observe(
  #   swi
  # )
  ns <- session$ns
  observe(
    updateSliderInput(
      session = session, inputId = "inp", step = 2,
      label = paste0("Day (", f_labels(Var2show), ")"), min = 1, max = length(unique(data$DateRep)), value = length(unique(data$DateRep))
    )
  )
  data_new <- reactive({
    data %>%
      filter(as.character(DateRep) %in% as.character(max(ordered(DateRep))), Countries != "World") %>%
      arrange(get(Var2show)) %>%
      tail(10)
  })

  data_2tal_cases <- reactive({
    data %>% filter(as.character(DateRep) %in% as.character(unique(data$DateRep)[input$inp]), Countries != "World")
  })
  #
  output$plt <- renderPlotly({
    df <- data_2tal_cases() %>%
      arrange(get(Var2show)) %>%
      tail(15) %>%
      mutate(Countries2 = factor(Countries, levels = Countries))
    data_new() %>%
      plot_ly(color = ~Countries, colors = "Paired") %>%
      add_data(df) %>%
      add_bars(x = formula(paste0("~", Var2show)), y = ~Countries2) %>%
      layout(
        xaxis = list(title = f_labels(Var2show)),
        yaxis = list(title = "")
      ) %>%
      add_annotations(
        text = formula(paste0("~", Var2show)),
        y = ~ reorder(Countries, get(paste0("rank_", Var2show))), x = ~ I(get(Var2show) + 0.05 * max(get(Var2show))), showarrow = FALSE
      ) %>%
      add_annotations(
        text = ~ max(ordered(DateRep)),
        x = ~ I(max(get(Var2show)) * 9 / 10), y = ~ I(1), font = list(size = 30), bordercolor = "black", showarrow = FALSE
      ) %>%
      hide_legend() %>%
      animation_opts(redraw = TRUE, mode = "afterall")
  })
}
ui <- fluidPage(
  ui_barCharts("f_1")
)
server <- function(input, output) {
  data <- DataDownload()
  data2 <- compute_cum_ranks(data)
  callModule(server_barCharts, "f_1", data2, "Cases")
}
shinyApp(ui, server)
