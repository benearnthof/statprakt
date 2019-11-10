#install.packages("dplyr")

dplyr::count(typisiert, Crowd)
#8825 Belege durch Crowdsourcing von insg. 86569 Belegen

typisiert2 <- subset(typisiert, typisiert$Crowd==1)
#Bilde Teilmenge mit Belegen durch Crowdsourcing

#install.packages(c("sf", "raster", "sp", "mapview"))
coords <- typisiert$Georeferenz
coords <- as.character(coords)

bsp <- sub("POINT", "", coords)
bsp <- sub("\\(", "", bsp)
bsp <- sub("\\)", "", bsp)

head(bsp)

list <- stringr::str_split(bsp, " ")
one <- sapply(list, `[[`, 1)
two <- sapply(list, `[[`, 2)

lat <- two
lng <- one

library("sp")
library("mapview")
library("raster")

points <- data.frame(lat = as.numeric(lat), lng = as.numeric(lng))
points <- na.omit(points)
coordinates(points) <- ~ lng + lat
crs(points) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mapview(points)
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


