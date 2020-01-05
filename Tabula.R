require(sf)
require(raster)
require(sp)
require(mapview)

tabula <- read.csv("tabula.csv", encoding = "UTF-8")
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
lat <- coords[[1]]
lng <- coords[[2]]
points <- data.frame(lat = as.numeric(lat), lng = as.numeric(lng))
coordinates(points) <- ~ lng + lat
crs(points) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
alpenraum <- readRDS(file = "alpenraum_polygon.RDS")

# points <- points[!is.na(over(points, alpenraum)),]
# nrow(points@coords)
plot(alpenraum)
plot(points, add = TRUE)

tabula <- read.csv("tabula.csv", encoding = "UTF-8", sep = ",")
x <- tabula$Geodaten
dta <- x[-length(x)]
coords_tabula <- get_coords(dta)
tabula <- tabula[-(490), ]
tabula$lat = coords_tabula$lat
tabula$lng = coords_tabula$lng
coordinates(tabula) <- ~ lng + lat

sp_crowd <- readRDS(file = "sp_crowd.RDS")
box <- sp_crowd@bbox
box[2,] <- c(44, 49)
box[,1] <- c(4.5, 43.3)
box[,2] <- c(16.5, 48.5)
map <- get_stamenmap(bbox = box, zoom = 6, maptype = "toner-lite") 
canvas <- ggmap(map)

gates <- readRDS(file = "alpenraum_polygon.RDS")

canvas + geom_point(data = as.data.frame.matrix(tabula@coords), aes(x = lng, y = lat)) +
  geom_polygon(data = broom::tidy(gates), aes(x = long, y = lat), col = "red",fill = NA)
# verwende unique points um polygon korrekt zu plotten. 