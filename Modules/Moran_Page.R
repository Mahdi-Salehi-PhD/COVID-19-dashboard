# Moran Modulde.
library(shiny)
library(shinyWidgets)
ui_moran <- function(id) {
  ns <- NS(id)
  tagList(
    radioGroupButtons(ns("contin"),
      choiceNames = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"),
      choiceValues = c("Africa", "Asia", "Europe", "North_America", "Oceania", "South_America"), ,
      selected = "Africa", status = "info",
      checkIcon = list(
        yes = tags$i(
          class = "fa fa-circle",
          style = "color: steelblue"
        ),
        no = tags$i(
          class = "fa fa-circle-o",
          style = "color: steelblue"
        )
      )
    ),
    tags$div(sliderInput(ns("threshold"),
      label = "Threshold for geographical distance",
      min = 0, max = 1, step = 0.05, value = .5, animate = FALSE
    ),
    width = "20px"
    ),
    tabsetPanel(
      tabPanel(
        "Charts",
        box(
          title = "The p-value of Moran's test", width = 22, status = "warning", solidHeader = TRUE,
          withSpinner(plotlyOutput(ns("plt1")))
        ),
        box(
          title = "Difference of observed and expected values of Moran's Index", width = 22, status = "warning", solidHeader = TRUE,
          plotlyOutput(ns("plt2"))
        )
      ),
      tabPanel(
        "Data Table",
        dataTableOutput(ns("tbl")),
        downloadButton(ns("down_csv"), label = "  .csv", width = "100px"),
        downloadButton(ns("down_json"), label = "  .json", width = "100px")
      )
    )
  )
}


server_moran <- function(input, output, session, data) {
  adjacency <- adjucent_Continent_data(adjucent_Country_data())[-1]
  distance <- adjucent_Continent_data(f_dist_Countries())[-1]
  Adj_mat <- reactive(adjacency[[input$contin]])
  Dis_mat <- reactive(distance[[input$contin]])

  M <- reactive(Moran_Index(
    Adj_mat = Adj_mat(), Dis_mat = Dis_mat(),
    data = data, Threshold = input$threshold
  ))

  output$plt1 <- renderPlotly(
    M()$plt1
  )
  output$plt2 <- renderPlotly(
    M()$plt2
  )

  output$tbl <- renderDataTable(
    datatable(M()$tbl)
  )

  output$down_csv <- downloadHandler(
    filename = function() {
      paste0("data", ".csv", sep = "")
    },
    content = function(file) {
      data.table::fwrite(M()$tbl, file)
    }
  )
  output$down_json <- downloadHandler(
    filename = function() {
      paste0("data", ".json", sep = "")
    },
    content = function(file) {
      jsonlite::write_json(M()$tbl, file)
    }
  )
}
