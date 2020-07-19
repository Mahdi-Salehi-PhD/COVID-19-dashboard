LGM <- function(
                yy, # country data
                ydate, # the corresponding date of y
                Region_name,
                upper = 50, # the upper bound of the forecaste
                conf = FALSE, # Adding a confidence interval around the forecaste
                modell) {
  I <- yy > 0
  y <- yy[I]
  DF <- data.frame(Date = seq_along(y), y = y)
  if (modell) {
    model <- with(DF, nls(y ~ SSlogis(Date, phi1, phi2, phi3)))
  } else {
    model <- with(DF, nls(y ~ SSgompertz(Date, b1, b2, b3)))
  }
  r_sq <- 1 - sum(residuals(model)^2) / sum((y - mean(y))^2)
  fore <- data.frame(Date = 1:(length(y) + upper), region = Region_name)
  Prediction <- predict(model, newdata = fore)
  fore$Prediction <- Prediction
  # at <- seq(1, nrow(fore), by = Step)
  if (conf) {
    xgrad <- attr(Prediction, "gradient")
    SD <- sqrt(diag(xgrad %*% vcov(model) %*% t(xgrad)))
    fore$lwr <- Prediction - 1.96 * SD
    fore$upr <- Prediction + 1.96 * SD
  }

  #---- adding zero Rows
  fore0 <- matrix(NA, nrow = sum(!I), ncol = (ncol(fore) - 1))
  # print(fore0)
  # print(colnames(fore))
  colnames(fore0) <- colnames(fore)[colnames(fore) != "region"]
  fore0 <- as.data.frame(fore0)
  fore0 <- cbind(fore0, region = Region_name)
  # print(colnames(fore0))
  #----
  fore <- rbind(fore0, fore)
  fore$day <- seq_along(fore$Date)
  fore <- cbind(fore, r_sq = r_sq)
  fore <- cbind(fore,
    y = c(yy, rep(NA, upper)),
    ydate = as.character(c(as.Date(ydate), max(as.Date(ydate)) + 1:upper))
  )
  return(fore)
}
LGM_plot <- function(
                     data,
                     Region_name,
                     upper = 50,
                     conf = FALSE,
                     Step = 10,
                     Log = FALSE,
                     modell) {
  YY <- split(data, data$Countries)
  YY[!names(YY) %in% Region_name] <- NULL

  f_LGM <- function(x, conf, upper, modell) {
    LGM(x$cum_cases, Region_name = unique(x$Countries), conf = conf, ydate = x$DateRep, upper = upper, modell = modell)
  }
  res <- lapply(YY, f_LGM, conf = conf, upper = upper, modell = modell)
  res <- do.call(rbind, res)
  res$region <- factor(res$region, levels = unique(res$region), labels = unique(res$region))

  p <- plot_ly() %>%
    add_markers(
      x = ~day, y = ~y, data = res, marker = list(size = 6), colors = "Dark2", legendgroup = ~region, showlegend = FALSE, color = ~region
    ) %>%
    add_lines(
      x = ~day, y = ~Prediction,
      name = ~ paste(region, "<br>", "<a style='font-size:11px;color:blue'>", "(R_sq: ", round(r_sq, 4), ")", "</a>"), legendgroup = ~region, colors = "Dark2", showlegend = TRUE,
      color = ~region, hoverinfo = "text", textposition = "auto",
      text = ~ paste("Prediction: ", ceiling(Prediction), "<br>", region)
    )

  if (conf) {
    p <- add_ribbons(p,
      x = ~day, ymin = ~lwr, ymax = ~upr, name = ~region,
      color = ~region, colors = "Dark2",
      legendgroup = ~region, alpha = .2, showlegend = FALSE, span = I(0),
      hoverinfo = "text", textposition = "auto", text = ~ paste(
        "upr: ", ceiling(upr),
        "<br> lwr: ", ceiling(lwr),
        "<br>", region
      )
    )
  }

  # tep = seq(from = 1,to = length(res[[1]]$day),by = Step)
  p <- p %>% layout(
    yaxis = list(type = ifelse(Log, "log", "linear"), title = list(text = "Cumulative Confirmed Cases")),
    xaxis = list(
      title = list(text = "Date Reported"),
      tickvals = ~ unique(day)[seq(from = 1, to = length(unique(day)), by = I(Step))],
      ticktext = ~ unique(ydate)[seq(from = 1, to = length(unique(ydate)), by = I(Step))], tickmode = "array"
    )
  )
  return(p)
}
# data <- DataDownload()
# LGM_plot(data = data, Region_name = c("World"),
#          conf = TRUE, Log = FALSE, upper = 100, Step = 50, modell = FALSE)
