# analyse von inschriften
# https://geocompr.robinlovelace.net/adv-map.html
# https://trucvietle.me/r/tutorial/2017/01/18/spatial-heat-map-plotting-using-r.html
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
inschriften <- read.csv("inschriften.csv", encoding = "UTF-8")
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

#in Datensatz Inschriften Spalte mit Breite und Laenge hinzufügen
inschriften$Laenge <- lng
inschriften$Breite <- lat

#map
points <- data.frame(lat = as.numeric(lat), lng = as.numeric(lng))
points <- distinct(points)
coordinates(points) <- ~ lng + lat
crs(points) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

counts <- as.data.frame(table(coords))
test <- sub("POINT\\(", "", counts$coords)
test <- sub("\\)", "", test)
list <- stringr::str_split(test, " ")
# fuege koordinaten dem datensatz hinzu
counts$lat <- sapply(list, `[[`, 2)
counts$lng <- sapply(list, `[[`, 1)
counts$lat <- as.numeric(counts$lat)
counts$lng <- as.numeric(counts$lng)

coordinates(counts) <- ~ lng + lat
crs(counts) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mapview(counts)

require(ggmap)

# erzeugen des canvas
sp_crowd <- readRDS(file = "sp_crowd.RDS")
box <- sp_crowd@bbox
box[2,] <- c(44, 49)
box[,1] <- c(4.5, 43.3)
box[,2] <- c(16.5, 48.5)
map <- get_stamenmap(bbox = box, zoom = 6, maptype = "toner-lite") 
canvas <- ggmap(map)

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

# datenimport fuer tabula
tabula <- read.csv("tabula.csv", encoding = "UTF-8", sep = ",")
tabula <- tabula[-nrow(tabula)]
x <- tabula$Geodaten
dta <- x#[-length(x)]
coords_tabula <- get_coords(dta)
# tabula <- tabula[-(490), ]
tabula$lat = coords_tabula$lat
tabula$lng = coords_tabula$lng
coordinates(tabula) <- ~ lng + lat

# erzeugen der karten: 
# einmal tabula mit inschriften 
# einmal nur inschriften 
tabula_inschriften <- canvas + geom_point(data = as.data.frame.matrix(points@coords), 
                                          aes(x = lng, y = lat), alpha = 0.5) +
  geom_point(data = as.data.frame.matrix(tabula@coords),
             aes(x = lng, y = lat), col = "red", size = 2)

ggsave("tabula_inschriften_map.png", plot = tabula_inschriften, 
       width = 18, height = 12, units = "cm")

inschriften <- tabula_inschriften <- canvas + geom_point(data = as.data.frame.matrix(points@coords), 
                                                         aes(x = lng, y = lat), alpha = 0.5)
ggsave("inschriften_ohne_tabula.png", plot = inschriften,
       width = 18, height = 12, units = "cm")
