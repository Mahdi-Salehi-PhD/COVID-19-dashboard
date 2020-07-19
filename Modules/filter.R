library(dplyr)

ntop_data <- function(data, n, filtertype) {
  # "case" "death" recovered "date_case" "date_death" date_recovered

  #####
  filter_ntop_case <- function(data, n) {
    data %>%
      group_by(Countries) %>%
      transmute(sum_cases = sum(Cases)) %>%
      arrange(-sum_cases) %>%
      distinct() %>%
      head(n)
  }
  # filter_ntopCountry_Cases(data,20)

  filter_ntop_death <- function(data, n) {
    data %>%
      group_by(Countries) %>%
      transmute(sum_death = sum(Deaths)) %>%
      arrange(-sum_death) %>%
      distinct() %>%
      head(n)
  }
  # filter_ntopCountry_recovered
  filter_ntop_recovered <- function(data, n) {
    data %>%
      group_by(Countries) %>%
      transmute(sum_recovered = sum(Recovered)) %>%
      arrange(-sum_recovered) %>%
      distinct() %>%
      head(n)
  }
  # filter_ntopCountry_Deaths(data,10)

  filter_ntop_date_case <- function(data, n) {
    data %>%
      group_by(DateRep) %>%
      transmute(sum_cases = sum(Cases)) %>%
      arrange(-sum_cases) %>%
      distinct() %>%
      head(n)
  }
  # filter_ntopDate_Cases(data,10)

  filter_ntop_date_death <- function(data, n) {
    data %>%
      group_by(DateRep) %>%
      transmute(sum_death = sum(Deaths)) %>%
      arrange(-sum_death) %>%
      distinct() %>%
      head(n)
  }
  filter_ntop_date_recovered <- function(data, n) {
    data %>%
      group_by(DateRep) %>%
      transmute(sum_recovered = sum(Recovered)) %>%
      arrange(-sum_recovered) %>%
      distinct() %>%
      head(n)
  }
  eval(call(paste0("filter_ntop_", filtertype), data, n))
}
