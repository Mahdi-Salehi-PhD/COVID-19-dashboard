library(jsonlite)
library(purrr)
library(dplyr)
library(stringr)
library(readxl)
DataDownload <- function() {
  data <- jsonlite::fromJSON("https://pomber.github.io/covid19/timeseries.json")
  names(data) <- f_Country_wrangling(names(data))


  f <- function(x, Countries) {
    Cases <- with(x, c(0, abs(diff(confirmed))))
    Deaths <- with(x, c(0, abs(diff(deaths))))
    Recovered <- with(x, c(0, abs(diff(recovered))))
    x$confirmed <- cumsum(Cases)
    x$deaths <- cumsum(Deaths)
    x$recovered <- cumsum(Recovered)
    names(x) <- c("DateRep", "cum_cases", "cum_death", "cum_recovered")
    cbind(Countries, x, Cases, Deaths, Recovered)
  }
  f2 <- function(x) {
    res <- data.frame()
    for (i in seq_along(x)) {
      res <- rbind(res, x[[i]])
    }
    return(res)
  }
  data2 <- map2(data, names(data), f)
  data3 <- f2(data2)

  f3 <- Vectorize(function(x) {
    xx <- unlist(strsplit(x, split = "-"))
    if (nchar(xx[2]) < 2) {
      xx[2] <- paste0("0", xx[2])
    }
    if (nchar(xx[3]) < 2) {
      xx[3] <- paste0("0", xx[3])
    }
    paste(xx, collapse = "-", sep = "")
  })
  f4 <- Vectorize(function(x) {
    xx <- unlist(strsplit(x, split = "-"))
    if (nchar(xx[2]) < 2) {
      xx[2] <- paste0("0", xx[2])
    }
    if (nchar(xx[3]) < 2) {
      xx[3] <- paste0("0", xx[3])
    }
    as.numeric(paste0(xx, collapse = ""))
  })
  data3$DateRep <- f3(data3$DateRep)
  data3$DateRep <- reorder(data3$DateRep, f4(data3$DateRep))
  names(data3$DateRep) <- NULL
  ###
  # adding world data

  data <- data3 %>%
    group_by(DateRep) %>%
    tidyr::nest()
  f_world <- function(x) {
    xnew <- x %>%
      summarise(
        Cases = sum(Cases), Deaths = sum(Deaths), Recovered = sum(Recovered),
        Countries = "World", cum_cases = sum(cum_cases), cum_death = sum(cum_death),
        cum_recovered = sum(cum_recovered)
      )
    rbind(x, xnew)
  }
  data$data <- lapply(data$data, f_world)
  data <- tidyr::unnest(data)


  ### adding population Data:
  pop <- fread("Modules/population.csv")
  pop <- tibble::add_row(pop, Countries = "World", Population = sum(pop$Population))
  pop$Countries <- f_Country_wrangling(pop$Countries)
  pop <- pop[, c("Countries", "Population")]
  res <- inner_join(data, pop, by = "Countries")
  res <- ungroup(res)
  res$Countries <- factor(res$Countries, levels = unique(res$Countries), labels = unique(res$Countries))
  return(res)
}
