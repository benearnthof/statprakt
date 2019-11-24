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




inschriften_map <- ggmap(get_stamenmap(bbox = c(left = 1, bottom = 42, 
                                      right = 20, top = 50), zoom = 7, maptype = "toner"))

i <-insch + geom_point(aes(x = lng , y = lat), colour = "#377eb8", data = df_insch, alpha = 0.35)+
  ggtitle("Fundorte von lat. Inschriften")
#i
# Tabula zur Map hinzufügen
i + geom_point(aes(x = lng , y = lat), colour = "#ff7f00", data = coords_tabula, alpha = 1, size = 2) +
  ggtitle("Vergleich: Inschriftenfunde und Tabula Peutingeriana")

#Heranzoomen
insch2 <- ggmap(get_stamenmap(bbox = c(left = 7.5, bottom = 43, 
                                       right = 20, top = 49), zoom = 7, maptype = "toner"))
i2 <-insch2 + geom_point(aes(x = lng , y = lat), colour = "#377eb8", data = df_insch, alpha = 0.35)+
  ggtitle("Fundorte von lat. Inschriften")
i2 + geom_point(aes(x = lng , y = lat), colour = "#ff7f00", data = coords_tabula, alpha = 1, size = 2) +
  ggtitle("Vergleich: Inschriftenfunde und Tabula Peutingeriana")


