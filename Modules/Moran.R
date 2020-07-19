library(ape)
library(ggplot2)
library(plotly)
source("Modules/weight_matrices.R")
Moran_Index <- function(
                        Adj_mat, # Adjacenct matrix
                        Dis_mat, # Distance matrix
                        data,
                        Threshold = 0.5 # in (0, 1)
) {
  I <- rownames(Dis_mat)[rownames(Dis_mat) %in% rownames(Adj_mat)]
  Dis_mat <- Dis_mat[I, I]
  Adj_mat <- Adj_mat[I, I]
  Sub_data <- filter(
    data[c("DateRep", "Countries", "Cases")], Countries %in% I
  )
  from <- which(Sub_data$DateRep == "2020-03-15")[1]
  Sub_data <- Sub_data[from:nrow(Sub_data), ]
  Sub_data[["DateRep"]] <- as.vector(Sub_data[["DateRep"]])
  slct <- I %in% Sub_data$Countries
  Adj_mat <- Adj_mat[slct, slct]
  Dis_mat <- Dis_mat[slct, slct]
  lambda <- quantile(Dis_mat[lower.tri(Dis_mat)], Threshold)
  alpha <- 1
  DIS_mat <- matrix(0, ncol(Dis_mat), ncol(Dis_mat))
  DIS_mat <- (1 / Dis_mat)^alpha * (Dis_mat <= lambda)
  diag(DIS_mat) <- 0
  report0 <- by(
    Sub_data, Sub_data["DateRep"],
    function(x) try(Moran.I(x$Cases, weight = Adj_mat))
  )
  report1 <- report0[!sapply(report0, is.null)]
  report <- sapply(
    do.call(rbind.data.frame, report1),
    function(x) as.double(as.character(x))
  )
  report0_D <- by(
    Sub_data, Sub_data["DateRep"],
    function(x) try(Moran.I(x$Cases, weight = DIS_mat))
  )
  report1_D <- report0_D[!sapply(report0_D, is.null)]
  report_D <- sapply(
    do.call(rbind.data.frame, report1_D),
    function(x) as.double(as.character(x))
  )

  #------------overall
  overall <- rbind.data.frame(report, report_D)
  rownames(overall) <- NULL
  overall$"Date Reported" <- rep(names(report1), 2)
  names(overall) <- c("Observed", "Expected", "SD", "p_value", "Date Reported")
  overall <- overall[c("Date Reported", "Observed", "Expected", "SD", "p_value")]
  overall$Weight <- gl(2, nrow(report), labels = c(
    "Measurement by adjacency",
    "Measurement by geographical distance"
  ))
  Date <- rep(1:(nrow(overall) / 2), 2)
  at <- seq(1, nrow(overall) / 2, by = 5)
  p <- ggplot(data = overall, aes(x = Date, y = p_value)) +
    geom_point(color = "purple") +
    geom_line() +
    geom_abline(intercept = 0.05, slope = 0, linetype = "dotted") +
    facet_wrap(~Weight, nrow = 2, scales = "free_y") +
    theme_bw() +
    theme(
      axis.text = element_text(angle = 40, hjust = 0.9, size = 10),
      panel.background = element_rect(fill = "gray97"),
      plot.margin = margin(.1, .1, .1, .1, "cm"),
      plot.background = element_rect(
        fill = "grey94",
        colour = "black",
        size = 1
      ),
      strip.text.x = element_text(
        size = 10, color = "blue", face = "bold"
      ),
      title = element_text(size = 10, face = "italic", vjust = 0.1, margin = unit(0.2, "cm"))
    ) +
    scale_y_continuous(expand = c(0.3, 0), name = "") +
    scale_x_continuous(name = "", breaks = at, labels = overall$"Date Reported"[at]) +
    labs(title = "The corresponding p-values of Moran's test over the time")
  p <- ggplotly(p) %>%
    layout(
      xaxis = list(
        title = "Date reported",
        titlefont = list(size = 13),
        tickvals = at, ticktext = overall$"Date Reported"[at],
        automargin = T
      )
    )
  q <- ggplot(data = overall, aes(x = Date, y = Observed - Expected)) +
    geom_point(color = "purple") +
    geom_line() +
    geom_abline(intercept = 0, slope = 0, linetype = "dotted") +
    facet_wrap(~Weight, nrow = 2, scales = "free_y") +
    theme_bw() +
    theme(
      axis.text = element_text(angle = 40, hjust = 0.9, size = 10),
      panel.background = element_rect(fill = "gray97"),
      plot.margin = margin(.1, .1, .1, .1, "cm"),
      plot.background = element_rect(
        fill = "grey94",
        colour = "black",
        size = 1
      ),
      strip.text.x = element_text(
        size = 10, color = "blue", face = "bold"
      ),
      title = element_text(size = 10, face = "italic", vjust = 0.1, margin = unit(0.2, "cm"))
    ) +
    scale_y_continuous(expand = c(0.3, 0), name = "") +
    scale_x_continuous(name = "", breaks = at, labels = overall$"Date Reported"[at]) +
    labs(title = "Differences of observed and expected values of Moran's Index over the time")
  q <- ggplotly(q) %>%
    layout(
      xaxis = list(
        title = "Date reported",
        titlefont = list(size = 13),
        tickvals = at, ticktext = overall$"Date Reported"[at],
        automargin = T
      )
    )
  overall[-c(1, 6)] <- round(overall[-c(1, 6)], 3)
  overall$p_value <- ifelse(overall$p_value < 1e-3, "< 1e-3", overall$p_value)
  return(list(tbl = overall, plt1 = p, plt2 = q))
}
