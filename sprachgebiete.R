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
# plot(test)

test <- as.character(test)
library(stringr)
locations <- str_locate_all(test, pattern = "\\([^()]+\\)")[[1]]
li <- list()
for (i in 1:nrow(locations)) {
  li[[i]] <- substr(test, start = locations[i,1], stop = locations[i,2])
}
for (i in 1:length(li)) {
  li[[i]] <- str_remove_all(li[[i]], pattern = c("\\(", "\\)"))
}

lst <- list()
for (i in 1:length(li)) {
  tmp <- str_split(li[[i]], ",")
  coords <- unlist(tmp)
  tmp <- str_split(coords, " ")
  lat <- sapply(tmp, `[[`, 1)
  lng <- sapply(tmp, `[[`, 2)
  points <- data.frame(lat = as.numeric(lat), lng = as.numeric(lng))
  points <- na.omit(points)
  coordinates(points) <- ~ lng + lat
  crs(points) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  lst[[i]] <- points
}


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
plot(romanic, add = F, pch = 20)
# nice
# rom       ger       sla       romger    gersla    romsla    romgersla
# rot       blau      gruen     lila      orange    gelb      braun
germanic <- gen_points(sprachgebiete$Geodaten[2])
plot(germanic, col = "red",add = TRUE, pch = 20)
slavic <- gen_points(sprachgebiete$Geodaten[3])
plot(slavic, col = "blue", add = TRUE, pch = 20)

sp_crowd <- readRDS("sp_crowd.RDS")
box <- sp_crowd@bbox
box[2,] <- c(44, 49)
box[,1] <- c(4.884782, 43.43132)
box[,2] <- c(16.47003, 48.36694)
saveRDS(box, "bbox.RDS")
require(ggmap)
gebietsmap <- get_stamenmap(bbox = box, zoom = 7, maptype = "toner") 
romanic_df <- as.data.frame.matrix(romanic@coords)
ggmap(gebietsmap)
romanicmap <- ggmap(gebietsmap) +
  geom_point(aes(x = lng, y = lat),
             data = romanic_df, color = "skyblue", size = 1, alpha = 0.5)
romanicmap

# lets wrap that shit in a function
colors <- c("#e41a1c", "#377eb8", "#ffff33", "#984ea3",
            "#4daf4a", "#ff7f00", "#000000")

ReamapR <- function(areas = sprachgebiete, cols = colors) {
  box <- readRDS("bbox.RDS")
  areamap <- get_stamenmap(bbox = box, zoom = 7, maptype = "toner") 
  areamap <- ggmap(areamap)
  for (i in seq_len(nrow(areas))) {
    df <- as.data.frame.matrix(gen_points(areas$Geodaten[i])@coords)
    areamap <- areamap +
      geom_point(aes(x = lng, y = lat),
                 data = df, color = cols[i], size = 1, alpha = 1)
  }
  areamap
}
test <- ReamapR()
test 
legendtest <- test + 
  scale_colour_manual(name = 'the colour', 
                      values = c("#e41a1c" = "#e41a1c",
                                "#377eb8" = "#377eb8",
                                "#ffff33" = "#ffff33",
                                "#984ea3" = "#984ea3",
                                "#4daf4a" = "#4daf4a",
                                "#ff7f00" = "#ff7f00",
                                "#000000" = "#000000"), 
                      labels = c("rom", "ger", "sla", "romger", "gersla", "romsla", "romgersla"))
legendtest
# funktioniert nicht

# lege radius um inschriften und zähle in diesen bufferzones um die inschriften die funde. 
# vergleiche germanischen & slavischen sprachraum
# finde für jeden roten punkt raus ob ein blauer in radius liegt. => dynamischer radius
# punktweise inschrift oder keine inschrift


require(spatialEco)
require(sp)
data(meuse)
coordinates(meuse) = ~x+y
sr1=Polygons(list(Polygon(cbind(c(180114, 180553, 181127, 181477, 181294, 181007, 180409,
                                  180162, 180114), c(332349, 332057, 332342, 333250, 333558, 333676,
                                                     332618, 332413, 332349)))),'1')
sr2=Polygons(list(Polygon(cbind(c(180042, 180545, 180553, 180314, 179955, 179142, 179437,
                                  179524, 179979, 180042), c(332373, 332026, 331426, 330889, 330683,
                                                             331133, 331623, 332152, 332357, 332373)))),'2')
sr3=Polygons(list(Polygon(cbind(c(179110, 179907, 180433, 180712, 180752, 180329, 179875,
                                  179668, 179572, 179269, 178879, 178600, 178544, 179046, 179110),
                                c(331086, 330620, 330494, 330265, 330075, 330233, 330336, 330004,
                                  329783, 329665, 329720, 329933, 330478, 331062, 331086)))),'3')
sr4=Polygons(list(Polygon(cbind(c(180304, 180403,179632,179420,180304),
                                c(332791, 333204, 333635, 333058, 332791)))),'4')
sr=SpatialPolygons(list(sr1,sr2,sr3,sr4))
srdf=SpatialPolygonsDataFrame(sr, data.frame(row.names=c('1','2','3','4'), PIDS=1:4, y=runif(4)))

plot(srdf)
points(meuse, pch=20)

pts.poly <- point.in.poly(meuse, srdf)
head(pts.poly@data)

test <- coords2Polygons(romanic@coords, ID = "romanic")
library(rgdal)
library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
holes <- remove.holes(test)
plot(holes)

# problem sind multipolygone => abändern von stringsplit und import einzelner polygone als 
# listen sollte funktionieren.
