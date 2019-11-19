require(sf)
require(raster)
require(sp)
require(mapview)

tabula <- tabula[1:(nrow(tabula) - 1),]

# x<- tabula$Geodaten
# dta <- x[-length(x)]

get_coords <- function(x) {
  tmp <- as.character(x)
  tmp <- sub("POINT", "", tmp)
  tmp <- sub("\\(", "", tmp)
  tmp <- sub("\\)", "", tmp)
  list <- stringr::str_split(tmp, " ")
  lng <- as.numeric(sapply(list, `[[`, 1))
  lat <- as.numeric(sapply(list, `[[`, 2))
  list(lng = lng, lat = lat)
}

coords <- get_coords(tabula$Geodaten)
#in Datensatz Inschriften Spalte mit Breite und Länge hinzufügen
tabula$Laenge <- coords[[1]]
tabula$Breite <- coords[[2]]

#map
points <- data.frame(lat = as.numeric(lat), lng = as.numeric(lng))
coordinates(points) <- ~ lng + lat
crs(points) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

counts <- as.data.frame(table(coords))
test <- sub("POINT\\(", "", counts$coords)
test <- sub("\\)", "", test)
list <- stringr::str_split(test, " ")

