# South_America <- c(
#   "Brazil", "Colombia", "Argentina",  "Peru",
#   "Venezuela", "Chile", "Ecuador", "Bolivia",
#   "Paraguay", "Uruguay", "Guyana",
#   "Suriname"
# )
# continent <- fread("Modules/countryContinent.csv")
# continent[["continent"]][continent[["country"]] %in% South_America] <-
#  "South_America"
# continent[["continent"]][continent[["continent"]] == "Americas"] <- "North_America"
# fwrite(continent, "Modules/countryContinent.csv")
#------Adjacency-----------
adjucent_Country_data <- function() {
  nickName <- readLines("Modules/nickName.txt")
  nickName <- do.call(rbind, strsplit(nickName, "\t"))
  colnames(nickName) <- c("nick", "Name")
  xname <- list(
    nickName,
    c("BL", "Saint Barthelemy"),
    c("BQ", "Caribbiean Netherlands"),
    c("CW", "Kingdom of the Netherlands"),
    c("EH", "Western Sahara"),
    c("GS", "South Georgia"),
    c("KP", "North Korea"),
    c("MF", "Saint Martin"),
    c("SS", "South Sudan"),
    c("SX", "Sint Marten"),
    c("TL", "East Timor"),
    c("XK", "Kosovo")
  )
  nickName <- do.call(rbind, xname)
  #-------------------------------------------------------------------
  a <- jsonlite::read_json("Modules/neighbors.json")
  na <- names(a)
  f <- function(x) {
    as.integer(na %in% x)
  }
  res_mat <- f(a$AD$neighbours)
  for (i in 2:length(a)) {
    res_mat <- rbind(res_mat, f(a[[i]][[2]]))
  }
  diag(res_mat) <- 1
  fName_func <- function(x) {
    val <- which(x == nickName[, 1])
    ifelse(length(val) == 0, NA, nickName[val, 2])
  }
  na2 <- mapply(fName_func, na)
  na2 <- f_Country_wrangling(na2)
  dimnames(res_mat) <- list(na2, na2)
  return(res_mat)
}
adjucent_Continent_data <- function(x) {
  continent <- fread("Modules/countryContinent.csv")
  continent <- continent[, c("country", "continent")]
  continent$country <- f_Country_wrangling(continent$country)

  f <- function(y) {
    y[y %in% rownames(x)]
  }

  tx <- tapply(continent$country, continent$continent, f)

  # tx[[1]]<- NULL

  resList <- vector("list", length(tx))
  names(resList) <- names(tx)
  for (i in seq_along(tx)) {
    resList[[i]] <- x[tx[[i]], tx[[i]]]
  }
  resList
}

#------Distance-----------
library(maps)
library(rworldmap)
library(rworldxtra)
library(rgeos)
library(stringr)
f_dist_Countries <- function() {
  world2 <- read.csv("Modules/country_data.csv")
  world2 <- world2[-1]
  world2$name <- f_Country_wrangling(as.character(world2$name))
  #----------------------
  # Computing distance
  #----------------------
  f <- function(x, y) {
    # long <- world2$longitude
    rad <- pi / 180
    R <- 6371 # earth radius in km.
    d_lat <- (x - world2$latitude) * rad
    # print(d_lat)
    d_long <- (y - world2$longitude) * rad
    #
    x <- x * rad

    lat <- world2$latitude * rad
    #
    a <- sin(d_lat / 2)^2 + cos(x) * cos(lat) * sin(d_long / 2)^2
    C <- 2 * atan2(sqrt(a), sqrt(1 - a))
    # print(C)
    R * C
  }

  dis <- purrr::map2(world2$latitude, world2$longitude, f)

  res_mat <- matrix(unlist(dis), ncol = length(dis))

  dimnames(res_mat) <- list(world2$name, world2$name)
  return(res_mat)
}
