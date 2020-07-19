# time serice visualization:
library(plotly)
library(dplyr)
library(shiny)
library(RColorBrewer)
library(shinycssloaders)
library(shinysky)
library(shinyWidgets)
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

ui_timeSeriesPlot <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("inp"), label = "Add/Remove regions", choices = c(1, 2), multiple = TRUE),
    flowLayout(
      prettyCheckbox(ns("help"), label = "Hint", value = FALSE),
      uiOutput(ns("log2show")),
      uiOutput(ns("regression2show"))
    ),
    conditionalPanel("input.help",
      ns = ns,
      HTML("<p style = 'color:blue;font-size = 30px;font-weight:bold'>
                     You can add different regions to the graph by typing the name of the region in the search bar above and then pressing enter. Remove a region by clicking on the its name and pressing the backspace key. If you click on a specific curve, more statistical details appear below for the corresponding region.
                          </p>")
    ),
    withSpinner(uiOutput(ns("uiplt")), type = 4),
    uiOutput(ns("pl"))
  )
}


server_timeSeriesPlot <- function(input, output, session, data, var2show, source = NULL, showLABEL = TRUE, n = NULL, showLog = FALSE) {
  if (is.null(n)) n <- 1
  if (is.null(source)) source <- "A"
  ns <- session$ns
  co <- reactive({
    switch(var2show,
      "Cases" = ntop_data(data, n, "case"),
      "Deaths" = ntop_data(data, n, "death"),
      "Recovered" = ntop_data(data, n, "recovered"),
      "cum_cases" = ntop_data(data, n, "case"),
      "cum_death" = ntop_data(data, n, "death"),
      "cum_recovered" = ntop_data(data, n, "recovered"),
      "date_case" = ntop_data(data, n, "date_case"),
      "date_death" = ntop_data(data, n, "date_death")
    )
  })

  observe({
    updateSelectInput(session = session, inputId = "inp", choices = unique(data$Countries), selected = co()$Countries)
  })
  country <- reactive(input$inp)
  new_data1 <- reactive({
    data %>%
      filter(Countries %in% country())
  })
  output$log2show <- renderUI({
    if (stringr::str_starts(var2show, "cum")) {
      prettyCheckbox(inputId = ns("Log"), label = "Logarithmic scale", value = FALSE)
    }
  })
  output$uiplt <- renderUI(
    tagList(
      conditionalPanel("!input.regression",
        plotlyOutput(ns("plt")),
        ns = ns
      ),

      conditionalPanel("input.regression",
        flowLayout(
          numericInput(ns("upper"), "Number of days to be forecasted", min = 1, max = 100, value = 60),
          numericInput(ns("step"), "Step of the x axis", min = 1, max = 50, value = 10),
          prettyCheckbox(ns("conf"), "Add 95% confidence interval", value = FALSE),
          radioGroupButtons(ns("modell"), choices = list(
            "Logistic" = TRUE,
            "Gompertz" = FALSE
          ), selected = TRUE)
        ),
        plotlyOutput(ns("regplt")),
        ns = ns
      )
    )
  )


  output$plt <- renderPlotly({
    new_data1() %>%
      select(DateRep, var2show, Countries) %>%
      mutate(Countries = factor(Countries, labels = unique(Countries))) %>%
      plot_ly(
        x = ~DateRep, y = ~ get(var2show), color = ~Countries,
        colors = "Dark2", no.white = TRUE, steps = 2, customdata = ~Countries, source = source
      ) %>%
      add_lines(marker = list(size = 6)) %>%
      layout(yaxis = list(title = f_labels(var2show), type = ifelse(input$Log, "log", "linear")), xaxis = list(title = "Date reported"))
  })
  new_data2 <- reactiveVal()
  observeEvent(event_data("plotly_click", source = source), {
    df <- data %>%
      filter(Countries %in% event_data("plotly_click", source = source)$customdata)
    new_data2(df)
  })
  output$pl <- renderUI(
    ui_Country_property(ns("f_1"))
  )

  observeEvent(event_data("plotly_click", source = source), callModule(server_Country_property, "f_1", new_data2, var2show, showLABEL = showLABEL, Log = input$Log))

  #-----------------------
  # regression:
  #-----------------------

  output$regression2show <- renderUI({
    if (stringr::str_starts(var2show, "cum") && showLog) {
      prettyCheckbox(
        ns("regression"),
        label = div(
          style = "font-size: 16px;color:purple;font-weight:bold",
          "Prediction of the future"
        ),
        value = FALSE
      )
    }
  })

  output$regplt <- renderPlotly({
    LGM_plot(data = data, Region_name = country(), upper = input$upper, conf = input$conf, Step = input$step, Log = input$Log, modell = input$modell)
    # LGM(new_data1()$cum_cases, new_data1()$DateRep, unique(new_data1()$Countries), upper = input$upper, conf = input$conf, Step = input$step,Log = input$Log)
  })
}
