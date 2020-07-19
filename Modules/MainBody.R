library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(plotlyGeoAssets)
#########

#########
ui_mainBody <- function(id, ProportionShow = TRUE) {
  ns <- NS(id)
  tagList(
    fluidRow(
      if (ProportionShow) {
        tagList(
          box(
            title = "New Cases (during the past 24 hours)",
            withSpinner(tagList(
              valueBoxOutput(ns("inf_this_day_case"), width = 4),
              valueBoxOutput(ns("inf_this_day_death"), width = 4),
              valueBoxOutput(ns("inf_this_day_recovered"), width = 4)
            ))
          ),
          box(
            title = "Total Cases", solidHeader = TRUE,
            withSpinner(tagList(
              valueBoxOutput(outputId = ns("inf_2tal_case"), width = 4),
              valueBoxOutput(outputId = ns("inf_2tal_death"), width = 4),
              valueBoxOutput(outputId = ns("inf_2tal_recovered"), width = 4)
            ))
          )
        )
      } else {
        tagList(
          box(
            title = "New Cases (during the past 24 hours)",
            withSpinner(tagList(
              valueBoxOutput(ns("inf_this_day_case"), width = 6),
              valueBoxOutput(ns("inf_this_day_death"), width = 6)
            ))
          ),
          box(
            title = "Total Cases", solidHeader = TRUE,
            withSpinner(tagList(
              valueBoxOutput(outputId = ns("inf_2tal_case"), width = 6),
              valueBoxOutput(outputId = ns("inf_2tal_death"), width = 6)
            ))
          )
        )
      }
    ),
    box(
      title = "Confirmed", width = 22, status = "warning", solidHeader = TRUE,
      column(
        width = 6,
        ui_barCharts(ns("Cases"))
      ),
      column(
        width = 6,
        ui_barCharts(ns("cum_cases"))
      )
    ),
    # Death barcharts
    box(
      title = "Death", width = 22, status = "warning", solidHeader = TRUE,
      column(
        width = 6,
        ui_barCharts(ns("Deaths"))
      ),
      column(
        width = 6,
        ui_barCharts(ns("cum_death"))
      )
    ),
    if (ProportionShow) {
      box(
        title = "Recovered", width = 22, status = "warning", solidHeader = TRUE,
        column(
          width = 6,
          ui_barCharts(ns("Recovered"))
        ),
        column(
          width = 6,
          ui_barCharts(ns("cum_recovered"))
        )
      )
    }
  )
  # recovered barchart
}

server_mainBody <- function(input, output, session, data, ProportionShow = TRUE) {
  ns <- session$ns




  data_summary <- reactive({
    data %>%
      filter(data$Countries == "World", as.character(max(ordered(data$DateRep))) == as.character(data$DateRep)) %>%
      select(-starts_with("rank"))
  })
  data_2 <- reactive({
    data %>%
      mutate(Recovered = round(Recovered, 2), cum_recovered = round(cum_recovered, 2))
  })

  output$inf_this_day_case <- renderValueBox({
    valueBox(
      value = tags$p(data_summary()$Cases, style = "font-size: 70%;"),
      subtitle = "Confirmed", color = "purple"
    )
  })
  output$inf_this_day_death <- renderValueBox({
    valueBox(
      value = tags$p(data_summary()$Deaths, style = "font-size: 70%;"),
      subtitle = "Death", color = "black"
    )
  })
  output$inf_this_day_recovered <- renderValueBox({
    valueBox(
      value = tags$p(round(data_summary()$Recovered, 2), style = "font-size: 70%;"),
      subtitle = "Recovered", color = "green"
    )
  })
  # 2tal value boxes
  output$inf_2tal_case <- renderValueBox({
    valueBox(
      value = tags$p(data_summary()$cum_cases, style = "font-size: 70%;"),
      subtitle = "Confirmed", color = "purple"
    )
  })
  output$inf_2tal_death <- renderValueBox({
    valueBox(
      value = tags$p(data_summary()$cum_death, style = "font-size: 70%;"),
      subtitle = "Death", color = "black"
    )
  })
  output$inf_2tal_recovered <- renderValueBox({
    valueBox(
      value = tags$p(round(data_summary()$cum_recovered, 2), style = "font-size: 70%;"),
      subtitle = "Recovered", color = "green"
    )
  })

  callModule(server_barCharts, "Cases", data_2(), "Cases")
  callModule(server_barCharts, "cum_cases", data_2(), "cum_cases")
  callModule(server_barCharts, "Deaths", data_2(), "Deaths")
  callModule(server_barCharts, "cum_death", data_2(), "cum_death")
  if (ProportionShow) {
    callModule(server_barCharts, "Recovered", data_2(), "Recovered")
    callModule(server_barCharts, "cum_recovered", data_2(), "cum_recovered")
  }
}
