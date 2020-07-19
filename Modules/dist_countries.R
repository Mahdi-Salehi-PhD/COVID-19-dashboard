library(maps)
library(rworldmap)
library(rworldxtra)
library(rgeos)
library(stringr)
f_dist_Countries <- function() {
  world2 <- read.csv("country_data.csv")
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
  # a <- f(1,1)
  # f(world2$lat,world2$long)

  dis <- purrr::map2(world2$latitude, world2$longitude, f)

  res_mat <- matrix(unlist(dis), ncol = length(dis))

  dimnames(res_mat) <- list(world2$name, world2$name)
  return(res_mat)
}
