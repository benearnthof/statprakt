# Map über Tabula

library("ggmap")

x <- tabula$Geodaten
dta <- x[-length(x)]

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

coords_tabula <- get_coords(dta)

#coords_insch <- get_coords(inschriften$Geodaten)
tabula <- tabula[-(490), ]   # löschen der letzten Zeile (ohne Geo-Angaben)
tabula
tabula$lat = coords_tabula$lat
tabula$lng = coords_tabula$lng
coordinates(tabula) <- ~ lng + lat

coords_tabula <- as.data.frame.matrix(tabula@coords)


#Map über Inschriften erstellen

inschriften_map <- ggmap(get_stamenmap(bbox = c(left = 1, bottom = 42, 
                                      right = 20, top = 50), zoom = 6, maptype = "toner-lite"))

inschriften_map <-inschriften_map + 
  stat_bin2d(mapping = aes(x = lng , y = lat), data = df_insch,bins = 100)+
  scale_fill_gradient(low ="darkseagreen", high = "darkblue", limits = c(0,300) ) +
  ggtitle("Fundorte von lat. Inschriften")
inschriften_map
# Grafik erstellen zu Inschriften
# ggsave("Inschriften_Map.png", plot =i , width = 16, height = 10, units = "cm")

#Inschriften Map (eingegränzter Bereich)

eingegrenzt_df_insch <- subset(df_insch, lng >= 8.00)
eingegrenzt_df_insch <- subset(eingegrenzt_df_insch, lat <= 49)
inschriften_map_eingegrenzt <- ggmap(get_stamenmap(bbox = c(left = 1, bottom = 42, 
                                                right = 20, top = 50), zoom = 6, maptype = "toner-lite"))

inschriften_map_eingegrenzt <-inschriften_map_eingegrenzt + 
  stat_bin2d(mapping = aes(x = lng , y = lat), data = eingegrenzt_df_insch, bins = 100)+
  scale_fill_gradient(low ="darkseagreen", high = "darkblue", limits = c(0,300) ) +
  ggtitle("Fundorte von lat. Inschriften")
inschriften_map_eingegrenzt

# Map: Tabula und Inschriften 
i_tabula <- ggmap(get_stamenmap(bbox = c(left = 1, bottom = 42, 
                                         right = 20, top = 50), zoom = 6, maptype = "toner-lite"))
i_tabula <- i_tabula + geom_point(aes(x = lng , y = lat, colour = "Inschriften"), data = df_insch, alpha = 0.35)   
i_tabula <- i_tabula + geom_point(aes(x = lng , y = lat, colour = "Tabula"), data = coords_tabula, alpha = 1, size = 2) +
            scale_colour_manual(name="Legende",
                                values=c(Tabula="#ff7f00", Inschriften="#377eb8"))+
            ggtitle("Vergleich: Inschriftenfunde und Tabula Peutingeriana")

# Grafik zur Map mit Tabule Orten und Inschriften
# Mit scale_colour_manual die Legende manuel hinzufügen 
ggsave("Inschriften_Tabula_Map.png", plot =i_tabula, width = 16, height = 10, units = "cm")


#Heranzoomen Map: Tabula & Inschriften
insch2 <- ggmap(get_stamenmap(bbox = c(left = 7.5, bottom = 43, 
                                       right = 20, top = 49), zoom = 6, maptype = "toner-lite"))
insch2 <- insch2 + geom_point(aes(x = lng , y = lat, colour = "Inschriften"), data = df_insch, alpha = 0.35)
insch2 <- insch2 + geom_point(aes(x = lng , y = lat, colour = "Tabula"), data = coords_tabula, alpha = 1, size = 2) +
          scale_colour_manual(name="Legende",
                              values=c(Tabula="#ff7f00", Inschriften="#377eb8"))+
          ggtitle("Vergleich: Inschriftenfunde und Tabula Peutingeriana") 
 

ggsave("Zoom_Inschriften_Tabula_Map.png", plot =insch2 , width = 16, height = 10, units = "cm")


# noch von interesse: Wie viele der Umgebungen der Punkte der Tabula enthalten 
# Punkte der Inschriftenfunde? Wenn ja, in welchen Radien um die Straßenpunkte 
# ist dies der Fall?

# inschriften aus teilmengen_nicht_rom
distinct_inschriften <- readRDS("distinct_inschriften.RDS")
coordinates(distinct_inschriften) <- ~ lng + lat
# sampling with buffer
bufferzone_counter <- function(distance = 100, centerpoints = tabula, points = distinct_inschriften) {
  # crs damit umwandeln zu spatial points funktioniert
  crs <-  CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  crs(centerpoints) <- crs
  # umwandeln zu spatial points damit sp::over funktioniert
  sp_center <- sp::SpatialPoints(coords = centerpoints@coords, proj4string = crs)
  utm_center <- spTransform(sp_center, CRS("+proj=utm +zone=32 ellps=WGS84"))
  sf_center <- st_as_sf(utm_center)
  # konstruieren von Bufferzonen mit gewünschtem Radius um die Punkte
  center_buff <- st_buffer(sf_center, dist = distance, nQuadSegs = 50)
  sp_polygons_buffer <- sf::as_Spatial(center_buff$geometry)
  crs(points) <- crs
  sp_points <- sp::SpatialPoints(coords = points@coords, proj4string = crs)
  # letzte conversion damit sp::over funktioniert
  sp_points_utm <- spTransform(sp_points, CRS("+proj=utm +zone=32 ellps=WGS84"))
  crs(sp_polygons_buffer) <- CRS("+proj=utm +zone=32 ellps=WGS84")
  # check welche der punkte in welches polygon fallen
  check <- sp::over(sp_points_utm, sp_polygons_buffer)
  check <- check[!is.na(check)]
  # tabelle die angibt welche zeilennummer aus tabula wie viele inschriftenfundorte
  # im festgelegten Radius enthält.
  tbl <- table(check)
  tbl
}

test <- bufferzone_counter()
sort(test)
test <- bufferzone_counter(2500)
sort(test)

nrow(tabula@data)
dimnames(test)$check
# die dimnames der tabelle die von bufferzone_counter returned werden stimmen 
# mit den zeilennummern der tabuladaten überein.
test <- as.data.frame(test)
head(test)
tab <- tabula
tab@data$Freq <- 0
test$check <- as.numeric(levels(test$check))[test$check]
tab@data$Freq[test$check] <- test$Freq
head(tab@data)
tab@data$Freq
all.equal(length(tab@data$Freq[tab@data$Freq != 0]), nrow(test))
# wrap everything in function 

augment_tabula <- function(buffertable, tab = tabula) {
  tmp <- as.data.frame(buffertable)
  tab@data$Freq <- 0
  tmp$check <- as.numeric(levels(tmp$check))[tmp$check]
  tab@data$Freq[tmp$check] <- tmp$Freq
  tab
}

buffertable <- bufferzone_counter(distance = 2500)
res <- augment_tabula(buffertable)
length(res@data$Freq[res@data$Freq != 0])

buffertable <- bufferzone_counter(distance = 100)
res <- augment_tabula(buffertable)
length(res@data$Freq[res@data$Freq != 0])


buffertable <- bufferzone_counter(distance = 10000)
res <- augment_tabula(buffertable)
length(res@data$Freq[res@data$Freq != 0])

# scheint zu funktionieren
# naechster schritt: karte erstellen mit den punkten der Tabula als punkte auf der
# karte je mehr einzigartige fundstellen gemacht wurden, desto größer bzw. farbe 
# ändern. 

data <- cbind(res@data, res@coords)
data$Group <- round(data$Freq, digits = -1)
insch <- ggmap(get_stamenmap(bbox = c(left = 1, bottom = 42, 
                                      right = 20, top = 50), zoom = 7, maptype = "toner"))
insch
saveRDS(insch, file = "canvas.RDS")
map <- insch + geom_point(data = data, aes(x = lng, y = lat, size = Group),
                          color = "red", alpha = 0.75) +
  ggtitle("Testtitle") +
  labs(size = "test") +
  scale_size(range = c(2, 6))
  
  
map

# wrap everything in function
plt_tabulamap <- function(augment_res, canvas = insch, title = "Testtitle", round = TRUE) {
  data <- cbind(augment_res@data, augment_res@coords)
  if (round) {
    data$Freq <- round(data$Freq, digits = -1)
  }
  map <- canvas + geom_point(data = data, aes(x = lng, y = lat, size = Freq),
                             color = "red", alpha = 0.15) + 
    ggtitle(title) +
    labs(size = "test") +
    scale_size(range = c(2, 7))
  
  map
}

buffertable <- bufferzone_counter(distance = 2500)
res <- augment_tabula(buffertable)
plt_tabulamap(res)

buffertable <- bufferzone_counter(distance = 1000)
res <- augment_tabula(buffertable)
plt_tabulamap(res, round = FALSE)


buffertable <- bufferzone_counter(distance = 10000)
res <- augment_tabula(buffertable)
plt_tabulamap(res)

buffertable <- bufferzone_counter(distance = 2500, centerpoints = gem_vor)
res <- augment_tabula(buffertable, tab = gem_vor)
plt_tabulamap(res, round = FALSE)
