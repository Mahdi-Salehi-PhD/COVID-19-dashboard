library(tibble)
library(dplyr)
library(data.table)
continent_data <- function(data) {
  continent <- fread("Modules/countryContinent.csv")
  continent <- continent[, c("country", "continent")]
  continent$country <- f_Country_wrangling(continent$country)
  continent <- tibble::add_row(continent, country = "World", continent = "World")
  names(continent) <- c("Countries", "Continent")
  new_data <- inner_join(data, continent, by = "Countries")

  res <- new_data %>%
    group_by(DateRep, Continent) %>%
    select_if(is.numeric) %>%
    summarise_all(sum)
  #
  names(res)[2] <- "Countries"
  res <- ungroup(res)
  return(res)
}
