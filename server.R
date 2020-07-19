# Mahdi Salehi et al. (2020)
# salehi2sms@gmail.com
# Note: Be ensure that you are connected to the internet

library(shiny)
library(DT)
library(shinycssloaders)

nvisitors <- reactiveVal(0)
server <- function(input, output) {
  nvisitors(isolate(nvisitors()) + 1)
  onSessionEnded(function(x) {
    nvisitors(isolate(nvisitors()) - 1)
  })
  output$txtOnline <- renderPrint(
    cat(paste0("  Online: ", nvisitors()))
  )
  data <- DataDownload()
  output$tblData <- renderDT(
    server = F,
    datatable(
      data,
      colnames = c(
        "Date Reported", "Country", "Cumulative confirmed cases", "Cumulative deaths",
        "Cumulative recoveries", "New cases", "New deaths", "New recoveries", "Population"
      ), extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        buttons = list(extend = "csv", text = "download"),
        filename = "data",
        exportOptions = list(
          modifier = list(page = "all")
        )
      )
    )
  )
  data_cont <- continent_data(data)
  observeEvent(input$tabs,
    switch(input$tabs,
      "mainPage_country" = callModule(server_mainBody_Page, "mainPage__country", data),
      "mainPage_continent" = callModule(server_mainBody_Page, "mainPage__continent", data_cont),
      "reg_Country" = callModule(server_regression, "RegCountry", data),
      "reg_Continent" = callModule(server_regression, "RegContinent", data_cont),
      "tim_Country" = callModule(server_timPage, "tim_CountryPage", data),
      "tim_Continent" = callModule(server_timPage, "tim_ContinentPage", data_cont),
      "map_ord_country" = callModule(server_leafMap_Page, "Ordinary_map", data),
      "moran" = callModule(server_moran_page, "moran", data)
    ),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
}
