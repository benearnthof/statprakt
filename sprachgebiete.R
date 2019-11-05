sprachgebiete <- read.csv("~/statprakt/sprachgebiete.csv")

# gibt es regionen wo crowdsourcing dominanter ist als woanders
# hat die lenkung funktioniert oder war es den leuten egal?
# wo gibt es häufungen? 
# wurden andere begriffe crowd gesourced als erhofft wurde?
# inschriften analysieren => in welchen regionen existieren inschriften?
# => visualisieren der Einträge => existieren heute in den gleichen Regionen 
# matching von ids auf gebiete etc => mail an projektpartner

library(sf)
test <- sprachgebiete$Geodaten[1]
plot(test)
sf::st_multipolygon(test)
test <- as.character(test)

bsp <- sub("MULTIPOLYGON\\(\\(\\(", "", test)

list <- stringr::str_split(bsp, ",")
coords <- unlist(list)
list <- stringr::str_split(coords, " ")

one <- sapply(list, `[[`, 1)
two <- sapply(list, `[[`, 2)

lat <- two
lng <- one

require("sp")
#require("mapview")
require("raster")
require("sf")

points <- data.frame(lat = as.numeric(lat), lng = as.numeric(lng))
points <- na.omit(points)
coordinates(points) <- ~ lng + lat
crs(points) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
plot(points)

# wrap everything in a function for easier handling

gen_points <- function(sprachgebiet) {
  tmp <- as.character(sprachgebiet)
  tmp <- sub("MULTIPOLYGON\\(\\(\\(", "", tmp)
  list <- stringr::str_split(tmp, ",")
  coords <- unlist(list)
  list <- stringr::str_split(coords, " ")
  lng <- sapply(list, `[[`, 1)
  lat <- sapply(list, `[[`, 2)
  points <- data.frame(lat = as.numeric(lat), lng = as.numeric(lng))
  points <- na.omit(points)
  sp::coordinates(points) <- ~ lng + lat
  raster::crs(points) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  points
}

romanic <- gen_points(sprachgebiete$Geodaten[1])
plot(romanic)
# nice
# rom       ger       sla       romger    gersla    romsla    romgersla
germanic <- gen_points(sprachgebiete$Geodaten[2])
plot(germanic)
slavic <- gen_points(sprachgebiete$Geodaten[3])
plot(slavic)

