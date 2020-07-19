ui_regression <- function(id) {
  ns <- NS(id)
  tagList(
    radioGroupButtons(
      inputId = ns("type_model"),
      label = "",
      choices = list(
        "Logistic growth model" = TRUE,
        "Gompertz growth model" = FALSE
      ),
      justified = TRUE,
      status = "primary", selected = TRUE
    ),
    boxPlus(
      title = "Fitted model", status = "warning", solidHeader = TRUE, closable = FALSE, collapsible = FALSE, collapsed = FALSE, width = 22,

      selectInput(ns("inp"), label = "Add/Remove regions", choices = c("World"), multiple = TRUE),
      flowLayout(
        numericInput(ns("upper"), label = "Number of days to be forecasted", value = 60, min = 1, max = 100),
        numericInput(ns("step"), label = "Step of x axis", min = 1, max = 100, value = 10, step = 1)
      ),
      flowLayout(
        shinyWidgets::prettyCheckbox(ns("conf"), label = "Add 95% confidence interval", value = TRUE),
        prettyCheckbox(ns("Log"), label = "Logarithmic scale", value = FALSE)
      ),
      withSpinner(plotlyOutput(ns("plt")), type = 4)
    )
  )
}

server_regression <- function(input, output, session, data) {
  observe({
    updateSelectInput(session = session, inputId = "inp", choices = unique(data$Countries), selected = "World")
  })
  output$plt <- renderPlotly({
    LGM_plot(
      data = data, Region_name = input$inp, upper = input$upper, conf = input$conf,
      Step = input$step, Log = input$Log, modell = input$type_model
    )
  })
}

ui <- fluidPage(ui_regression("foad"))
server <- function(input, output) {
  callModule(server_regression, "foad", data)
}
shinyApp(ui, server)
