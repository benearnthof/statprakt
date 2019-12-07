sprachgebiete <- read.csv("~/statprakt/sprachgebiete.csv")

# gibt es regionen wo crowdsourcing dominanter ist als woanders
# hat die lenkung funktioniert oder war es den leuten egal?
# wo gibt es häufungen? 
# wurden andere begriffe crowd gesourced als erhofft wurde?
# inschriften analysieren => in welchen regionen existieren inschriften?
# => visualisieren der Einträge => existieren heute in den gleichen Regionen 
# matching von ids auf gebiete etc => mail an projektpartner

# rom       ger       sla       romger    gersla    romsla    romgersla
# rot       blau      gelb     lila      gruen      orange      schwarz
library(sf)
test <- sprachgebiete$Geodaten[7]
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
  lat <- sapply(tmp, `[[`, 2)
  lng <- sapply(tmp, `[[`, 1)
  points <- data.frame(lat = as.numeric(lat), lng = as.numeric(lng))
  points <- na.omit(points)
  coordinates(points) <- ~ lng + lat
  crs(points) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  lst[[i]] <- points
}

box <- sp_crowd@bbox
box[2,] <- c(44, 49)
box[,1] <- c(4.5, 43.3)
box[,2] <- c(16.5, 48.5)
coordinates(lst[[1]])
map <- get_stamenmap(bbox = box, zoom = 6, maptype = "toner-lite") 
df <- as.data.frame.matrix(lst[[1]]@coords)
df <- distinct(df)
canvas <- ggmap(map)
res1 <- canvas +
  geom_polygon(aes(x = lng, y = lat),
             data = df, color = "red", fill = "red")
res1
df <- as.data.frame.matrix(lst[[2]]@coords)
df <- distinct(df)
res <- res +
  geom_polygon(aes(x = lng, y = lat),
               data = df, color = "green", fill = "green",  inherit.aes = FALSE)
res

ggplot(data = df) +
  geom_polygon(aes(x = lng, y = lat), color = "red", fill = "red")
# lengths <- numeric(length = 334)
# for (i in 1:334) {
#   lengths[i] <- nrow(lst[[i]]@coords)
# }

# ugh <- list()
# ugh <- lst[which(lengths > 100)]

# for (i in 12) {
#   df <- as.data.frame.matrix(ugh[[i]]@coords)
#   df <- distinct(df)
#   res <- res1 + 
#     geom_polygon(aes(x = lng, y = lat), color = "green", fill = "green",
#                  data = df)
# }
# res
# 
# listone <- lst
# listtwo <- ugh
# listtre <- lst
# listfor <- lst 
# listfiv <- lst
# listsix <- lst
# listsev <- lst
# in speicher lesen via readRDS => preprocessing wurde für alle 7 multipolygone 
# manuell gemacht, da multipolygon 2 334 eintraege enthielt wurden nur die 14 
# polygone gespeichert die mehr als 100 vertices haben. 
# saveRDS(listone, "listone.RDS")
# saveRDS(listtwo, "listtwo.RDS")
# saveRDS(listtre, "listtre.RDS")
# saveRDS(listfor, "listfor.RDS")
# saveRDS(listfiv, "listfiv.RDS")
# saveRDS(listsix, "listsix.RDS")
# saveRDS(listsev, "listsev.RDS")

one <- readRDS("listone.RDS")
two <- readRDS("listtwo.RDS")
tre <- readRDS("listtre.RDS")
fou <- readRDS("listfor.RDS")
fiv <- readRDS("listfiv.RDS")
six <- readRDS("listsix.RDS")
sev <- readRDS("listsev.RDS")

mappr <- function(can = canvas) {
  colors <- c("#e41a1c", "#377eb8", "#ffff33", "#ff00ff",
            "#4daf4a", "#ff7f00", "#000000")
  areas <- list(one, two, tre, fou, fiv, six, sev)
  map <- can
  for (i in seq_along(areas)) {
    for (j in seq_along(areas[[i]])) {
      df <- as.data.frame.matrix(areas[[i]][[j]]@coords)
      df <- distinct(df)
      map <- map +
      geom_polygon(aes(x = lng, y = lat),
                 data = df, color = colors[i], fill = colors[i])
    }
  }
map
}

test <- mappr()
ggsave("sprachgebietsmap.png", plot = test, width = 16, height = 9, units = "cm")
dta <- distinct(as.data.frame.matrix(one[[1]]@coords))
dta <- as.matrix.data.frame(dta)
colnames(dta) <- c("x", "y")
poly <- coords2Polygons(dta, ID = "A")
plot(poly)

dta <- distinct(as.data.frame.matrix(two[[1]]@coords))
dta <- as.matrix.data.frame(dta)
colnames(dta) <- c("x", "y")
poly2 <- coords2Polygons(dta, ID = "B") 
plot(poly2)

wot <- rbind(poly, poly2)
plot(wot)
agg <- raster::aggregate(wot)
plot(agg)

# wrapping stuff in functions is what i do best

areas <- list(one, two, tre, fou, fiv, six, sev)
# deriving empty polygon we can use as basis for rbind
tmp <- poly
tmp@polygons <- list()

listaggreg8r <- function(areas) {
  poly <- SpatialPolygons(list())
  for (i in seq_along(areas)) {
    for (j in seq_along(areas[[i]])) {
      df <- as.data.frame.matrix(areas[[i]][[j]]@coords)
      df <- distinct(df)
      df <- as.matrix.data.frame(df)
      colnames(df) <- c("x", "y")
      tmp <- coords2Polygons(df, ID = paste0(i, j))
      poly <- rbind(poly, tmp)
    }
  }
  return(poly)
}

g8r <- listaggreg8r(areas = areas)
plot(g8r)
plot(raster::aggregate(g8r))

gates <- raster::aggregate(g8r)
crs(gates) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# lets see what points fall into the area at all
pnts_insch <- get_coords(inschriften$Geodaten)

points <- data.frame(lat = as.numeric(pnts_insch$lat), lng = as.numeric(pnts_insch$lng))
coordinates(points) <- ~ lng + lat
crs(points) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

l8r <- sp::over(points, gates)
table(l8r)

l8r_g8r <- points[!is.na(over(points, gates)),]
nrow(l8r_g8r@coords)

plot(gates)
plot(l8r_g8r, add = TRUE)
