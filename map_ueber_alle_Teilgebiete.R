#Map über alle Teilmengen der sprachgebiete ohne Inschriften Orte:
vorrömisch <- subset(nicht_rom, Sprache_Basistyp == "vor")

lateinisch <- subset(nicht_rom, Sprache_Basistyp == "lat")

#Bilden von weiteren vier Teilmengen
gem_vor <- subset(vorrömisch, morph_Typ_Sprache == "gem")
sla_vor <- subset(vorrömisch, morph_Typ_Sprache == "sla")
gem_lat <- subset(lateinisch, morph_Typ_Sprache == "gem")
sla_lat <- subset(lateinisch, morph_Typ_Sprache == "sla")

library("ggmap")
# MAP ger_vor

coords1 <- get_coords(gem_vor$Georeferenz)

gem_vor$lat = coords1$lat
gem_vor$lng = coords1$lng
coordinates(gem_vor) <- ~ lng + lat

df <- as.data.frame.matrix(gem_vor@coords)

gv <- ggmap(get_stamenmap(bbox = c(left = 5, bottom = 43.4, 
                                   right = 18, top = 49.5), zoom = 7, maptype = "toner"))
gv <- gv + geom_point(aes(x = lng , y = lat), colour = "#e41a1c", data = df, alpha = 0.35)+
  ggtitle("Orte germanischer Belege (morphologischer Typ) mit Basistyp vorrömisch")

# MAP  ger_lat
coords2 <- get_coords(gem_lat$Georeferenz)

gem_lat$lat = coords2$lat
gem_lat$lng = coords2$lng

coordinates(gem_lat) <- ~ lng + lat

df_gl <- as.data.frame.matrix(gem_lat@coords)

gl <- ggmap(get_stamenmap(bbox = c(left = 5, bottom = 43.4, 
                                   right = 18, top = 49.5), zoom = 7, maptype = "toner"))
gl <- gl + geom_point(aes(x = lng , y = lat), colour = "#ff7f00", data = df_gl, alpha = 0.35)

# MAP sla_vor
coords3 <- get_coords(sla_vor$Georeferenz)

sla_vor$lat = coords3$lat
sla_vor$lng = coords3$lng
coordinates(sla_vor) <- ~ lng + lat

df_sv <- as.data.frame.matrix(sla_vor@coords)

sv <- ggmap(get_stamenmap(bbox = c(left = 5, bottom = 43.4, 
                                   right = 18, top = 49.5), zoom = 7, maptype = "toner"))
sv <- sv + geom_point(aes(x = lng , y = lat), colour = "#4daf4a", data = df_sv, alpha = 0.35)

# MAP sla_lat
coords4 <- get_coords(sla_lat$Georeferenz)

sla_lat$lat = coords4$lat
sla_lat$lng = coords4$lng
coordinates(sla_lat) <- ~ lng + lat

df_sl <- as.data.frame.matrix(sla_lat@coords)

sl <- ggmap(get_stamenmap(bbox = c(left = 5, bottom = 43.4, 
                                   right = 18, top = 49.5), zoom = 7, maptype = "toner"))
sl <- sl + geom_point(aes(x = lng , y = lat), colour = "#984ea3", data = df_sl, alpha = 0.35)


# MAP über alle 4 Bereiche7
gesamt <- ggmap(get_stamenmap(bbox = c(left = 5, bottom = 43.4, 
                                   right = 18, top = 49.5), zoom = 7, maptype = "toner"))
gesamt <- gl + 
          geom_point(aes(x = lng , y = lat), colour = "#e41a1c", data = df, alpha = 0.35) +
          geom_point(aes(x = lng , y = lat), colour = "#984ea3", data = df_sl, alpha = 0.35) +
          geom_point(aes(x = lng , y = lat), colour = "#4daf4a", data = df_sv, alpha = 0.35)
