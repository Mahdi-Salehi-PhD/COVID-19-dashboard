# for leaflet plot:
library(ggmap)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(maps)
library(rworldmap)
library(rworldxtra)
leafMap_data <- function(data, date) {
  f_leaf_data <- function(data) {
    wmap <- getMap(resolution = "low")
    centroid <- rgeos::gCentroid(wmap, byid = TRUE)
    as.data.frame(centroid) -> world2
    world2 <- cbind(rownames(world2), world2)
    names(world2) <- c("Countries", "long", "lat")
    world2$Countries <- f_Country_wrangling(as.character(world2$Countries))

    geo <- which(unique(data$GeoId) %in% world2$Countries)
    world2$Countries[which(world2$Countries %in% data$GeoId)] <- unique(data$Countries)[geo]
    inner_join(world2, data, by = "Countries") %>%
      select(Countries, DateRep, long, lat, Deaths, Cases, cum_cases, cum_death, Recovered, cum_recovered)
  }
  f_leaf_data(data) %>%
    filter(as.character(DateRep) == as.character(unique(data$DateRep)[date]))
}
