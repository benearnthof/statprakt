require(sf)
require(raster)
require(sp)
require(mapview)
coords <- crowd01$Georeferenz
coords <- as.character(coords)

geo <- sub("POINT", "", coords)
geo <- sub("\\(", "", geo)
geo <- sub("\\)", "", geo)

list2 <- stringr::str_split(geo, " ")
one <- sapply(list2, `[[`, 1)
two <- sapply(list2, `[[`, 2)

Breite_crowd01 <- two
Länge_crowd01 <- one

Breite_crowd01 <- as.numeric(lat2)

Länge_crowd01 <- as.numeric(lng2)

mean(Breite_crowd01)
#47.18558
mean(Länge_crowd01)
#11.11622
#Mittelpunkt(11.11622 47.18558)