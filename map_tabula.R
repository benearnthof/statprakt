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
                                      right = 20, top = 50), zoom = 7, maptype = "toner"))

i <-insch + geom_point(aes(x = lng , y = lat), colour = "#377eb8", data = df_insch, alpha = 0.35)+
  ggtitle("Fundorte von lat. Inschriften")

# Grafik erstellen zu Inschriften
# ggsave("Inschriften_Map.png", plot =i , width = 16, height = 10, units = "cm")


# Map: Tabula und Inschriften 
i_tabula <- ggmap(get_stamenmap(bbox = c(left = 1, bottom = 42, 
                                         right = 20, top = 50), zoom = 7, maptype = "toner"))
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
                                       right = 20, top = 49), zoom = 7, maptype = "toner"))
insch2 <- insch2 + geom_point(aes(x = lng , y = lat, colour = "Inschriften"), data = df_insch, alpha = 0.35)
insch2 <- insch2 + geom_point(aes(x = lng , y = lat, colour = "Tabula"), data = coords_tabula, alpha = 1, size = 2) +
          scale_colour_manual(name="Legende",
                              values=c(Tabula="#ff7f00", Inschriften="#377eb8"))+
          ggtitle("Vergleich: Inschriftenfunde und Tabula Peutingeriana") 
 

ggsave("Zoom_Inschriften_Tabula_Map.png", plot =insch2 , width = 16, height = 10, units = "cm")


