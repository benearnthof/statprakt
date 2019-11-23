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
                 data = df, color = cols[i], size = 1, alpha = 0.5)
  }
  areamap
}
test <- ReamapR()
test
