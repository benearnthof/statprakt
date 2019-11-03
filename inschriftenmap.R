# analysis of inscriptions
# https://geocompr.robinlovelace.net/adv-map.html
# https://trucvietle.me/r/tutorial/2017/01/18/spatial-heat-map-plotting-using-r.html
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
inschriften <- read.csv("~/statprakt/inschriften.csv", encoding = "UTF-8")
require(sf)
require(raster)
require(sp)
require(mapview)
coords <- inschriften$Geodaten
coords <- as.character(coords)

test <- sub("POINT", "", coords)
test <- sub("\\(", "", test)
test <- sub("\\)", "", test)

list <- stringr::str_split(test, " ")
one <- sapply(list, `[[`, 1)
two <- sapply(list, `[[`, 2)

lat <- two
lng <- one

points <- data.frame(lat = as.numeric(lat), lng = as.numeric(lng))
coordinates(points) <- ~ lng + lat
crs(points) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

counts <- as.data.frame(table(coords))
test <- sub("POINT\\(", "", counts$coords)
test <- sub("\\)", "", test)
list <- stringr::str_split(test, " ")
# add the coords to the counts data frame
counts$lat <- sapply(list, `[[`, 2)
counts$lng <- sapply(list, `[[`, 1)
counts$lat <- as.numeric(counts$lat)
counts$lng <- as.numeric(counts$lng)

coordinates(counts) <- ~ lng + lat
crs(counts) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mapview(counts)

require(ggmap)
