# leaflet shows:
library(leaflet)
library(leaflet.providers)
library(ggmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(shiny)
library(sf)
library(rgeos)
library(shinycssloaders)
grow1 <- function(x, k) ifelse(k == 1, return(x), return(c(x, 1.5 * grow1(x, k - 1))))
grow2 <- function(x, k) ifelse(k == 1, return(x), return(c(x, 1.4 * grow2(x, k - 1))))

switch_labels <- function(x) {
  switch(x,
    "case" = return("Cumulative confirmed cases"),
    "death" = return("Cumulative deaths"),
    "recovered" = return("Cumulative recoveries")
  )
}

ui_leafMap <- function(id) {
  ns <- NS(id)
  tagList(
    withSpinner(leafletOutput(ns("leafMap"), height = 900), type = 4)
  )
}

server_leafMap <- function(input, output, session, data, var2show = "case") {
  ns <- session$ns
  f_col <- function(x) {
    switch(x,
      "case" = return("purple"),
      "death" = return("black"),
      "recovered" = return("green")
    )
  }
  f_cat <- function(x, type = "case") {
    f1 <- function(x) {
      as.numeric(cut(x, c(-0.1, 1.1, grow1(10, 35)), labels = 0:35)) - 1
    }
    f2 <- function(x) {
      as.numeric(cut(x, c(-0.1, 1.1, grow2(3, 40)),
        labels = 0:40
      )) - 1
    }
    switch(type, # case , death recovered
      "case" = f1(x$cum_cases),
      "death" = f2(x$cum_death),
      "recovered" = f1(x$cum_recovered)
    )
  }

  world <- reactive(ne_countries(scale = "small", returnclass = "sf"))

  data3 <- reactive({
    leafMap_data(data, input$inp_slider)
  })
  data4 <- reactive({
    leafMap_data(data, length(unique(data$DateRep)))
  })
  xl <- 1
  LEAF <- reactive({
    world() %>%
      leaflet(options = leafletOptions(minZoom = 2, maxZoom = 4)) %>%
      addTiles() %>%
      addControl(html = tagList(
        dropdownButton(
          size = "xs", icon = icon("gear"), label = "Slider",
          width = "300px", status = "warning", circle = F,
          tooltip = TRUE,
          sliderInput(ns("inp_slider"),
            label = paste0("Day (", switch_labels(var2show), ")"),
            min = 1, max = length(unique(data$DateRep)), step = 2,
            value = length(unique(data$DateRep)), animate = TRUE
          )
        )
      ), position = "topleft") %>%
      addMarkers(lng = 55.0163, lat = 36.4062, label = "Shahrood University of Technology", labelOptions = labelOptions(textsize = "15px")) %>%
      addMarkers(lng = 58.7911, lat = 36.2549, label = "University of Neyshabur", labelOptions = labelOptions(textsize = "15px")) %>%
      addMarkers(lng = 28.231437, lat = -25.754056, label = "University of Pretoria", labelOptions = labelOptions(textsize = "15px")) %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })

  zoom <- reactive({
    ifelse(is.null(input$leafMap_zoom), 2, input$leafMap_zoom)
  })
  center <- reactive({
    if (is.null(input$leafMap_center)) {
      c(179.462, -20.64275)
    } else {
      return(input$leafMap_center)
    }
  })
  observeEvent(input$inp_slider, {
    leafletProxy(ns("leafMap"), session = session, deferUntilFlush = T, data = data3()) %>%
      clearShapes() %>%
      removeControl("f_date") %>%
      setView(
        lng = center()$lng,
        lat = center()$lat,
        zoom = zoom()
      ) %>%
      addCircles(data3()$long, data3()$lat,
        weight = (f_cat(data3(), var2show) * 1.3),

        label = paste0(
          data3()$Countries, ":\n", "Confirmed: ",
          data3()$cum_cases, ",\n", "Death: ", data3()$cum_death, ",\n", "Recovered: ",
          round(data3()$cum_recovered, 2)
        ),

        popup = paste0(
          data3()$Countries, ":\n", "Confirmed: ",
          data3()$cum_cases, ",\n", "Death: ", data3()$cum_death, ",\n", "Recovered: ",
          round(data3()$cum_recovered, 2)
        ),

        color = f_col(var2show), opacity = .6,
        labelOptions = labelOptions(textsize = "20px")
      ) %>%
      addControl(HTML(paste0(
        "<p style = 'color:orange;font-size:23px;font-weight:bold'> Date: ",
        unique(data$DateRep)[input$inp_slider], "</p>"
      )),
      layerId = "f_date",
      position = "topright"
      )
  })
  output$leafMap <- renderLeaflet({
    LEAF()
  })
}
