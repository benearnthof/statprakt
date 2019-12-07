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
test
ggsave("sprachgebietsmap.png", plot = test, width = 17, height = 9, units = "cm")
