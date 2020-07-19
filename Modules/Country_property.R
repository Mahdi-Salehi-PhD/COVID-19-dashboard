library(shiny)
library(shinysky)
library(shinydashboardPlus)
ui_Country_property <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("pl"))
  )
}

server_Country_property <- function(input, output, session, data, var2show, showLABEL = TRUE, Log = FALSE) {
  ns <- session$ns
  output$pl <- renderUI(
    if (nrow(data()) > 0) {
      tagList(
        tagList(
          box(
            title = "Descriptive Statistics", boxToolSize = "lg", width = 22, collapsible = TRUE, background = "light-blue",
            # HTML("<center style = 'color:blue;font-size:20px;font-weight:bold'>Descriptive Statistics</center>"),
            hotable(ns("tbl")),
            plotlyOutput(ns("plt2"))
          )
        )
      )
    }
  )

  output$plt2 <- renderPlotly({
    if (showLABEL) {
      data() %>%
        plot_ly(x = ~DateRep) %>%
        add_lines(
          y = formula(paste0("~", ifelse(str_starts(var2show, "cum"), "cum_cases", "Cases"))),
          name = "Confirmed", marker = list(size = 6)
        ) %>%
        add_lines(
          y = formula(paste0("~", ifelse(str_starts(var2show, "cum"), "cum_death", "Deaths"))),
          name = "Deaths", marker = list(size = 6)
        ) %>%
        layout(
          # paper_bgcolor = "gray",
          yaxis = list(title = "", type = ifelse(Log, "log", "linear")),
          # plot_bgcolor = "lightblue",
          legend = ~ list(title = list(
            text = paste0("Region: ", as.character(unique(Countries))),
            font = list(size = 20, color = "red")
          )),

          title = list(text = "Per 1M", font = list(size = 15, color = "black"), x = 0.01, y = .98)
        )
    } else {
      data() %>%
        plot_ly(x = ~DateRep) %>%
        add_lines(
          y = formula(paste0("~", ifelse(str_starts(var2show, "cum"), "cum_cases", "Cases"))),
          name = "Confirmed", marker = list(size = 6)
        ) %>%
        add_lines(
          y = formula(paste0("~", ifelse(str_starts(var2show, "cum"), "cum_death", "Deaths"))),
          name = "Deaths", marker = list(size = 6)
        ) %>%
        add_lines(
          y = formula(paste0("~", ifelse(str_starts(var2show, "cum"), "cum_recovered", "Recovered"))),
          name = "Recovered", marker = list(size = 6)
        ) %>%
        layout(
          # paper_bgcolor = "lightblue",
          yaxis = list(title = "", type = ifelse(Log, "log", "linear")),
          xaxis = list(title = "Date reported"), # plot_bgcolor = "lightblue",
          legend = ~ list(title = list(
            text = paste0("Region: ", as.character(unique(Countries))),
            font = list(size = 20, color = "red")
          ))
        )
    }
  })

  output$tbl <- renderHotable({
    f <- function(x) {
      xday <- min(which(x != 0))
      if (is.infinite(xday)) {
        # x <- x[xday:length(x)]
        c(
          Total = NA, SQ = NA,
          median = NA, mean = NA, SQ = NA,
          max = NA, sd = NA,
          day = NA, xday = NA
        )
      } else {
        day <- which.max(x)
        x <- x[xday:length(x)]
        c(
          Total = sum(x), SQ = quantile(x, 0.25),
          median = median(x), mean = mean(x), SQ = quantile(x, .75),
          max = max(x), sd = round(sd(x), 2),
          day = day, xday = xday
        )
      }
    }
    if (showLABEL) {
      df <- data() %>%
        select(Cases, Deaths) %>%
        sapply(f) %>%
        t() %>%
        as.data.frame()
    } else {
      df <- data() %>%
        select(Cases, Deaths, Recovered) %>%
        sapply(f) %>%
        t() %>%
        as.data.frame()
    }

    cbind(
      Region = data()$Countries[1:nrow(df)], Feature = rownames(df), df[-c(length(df), length(df) - 1)],
      Date_of_1st_case = (data()$DateRep[df$xday]), Date_of_peak = data()$DateRep[df$day]
    )
  })
}

ui <- fluidPage(
  ui_Country_property("f_1")
)
server <- function(input, output) {
  data <- DataDownload()
  data2 <- compute_cum_ranks(data)
  data3 <- reactive({
    data2 %>%
      filter(Countries %in% "Iran")
  })
  callModule(server_Country_property, "f_1", data3, "cum_cases")
}
shinyApp(ui, server)
