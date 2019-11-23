table(nicht_rom$morph_Typ_Sprache)
#gem  sla 
#8971 1305 

table(nicht_rom$Sprache_Basistyp)
#lat  vor 
#7564 2712 

#Bilden von zwei Teilmengen (vorrömisch und lateinisch)
vorrömisch <- subset(nicht_rom, Sprache_Basistyp == "vor")

lateinisch <- subset(nicht_rom, Sprache_Basistyp == "lat")

#Bilden von weiteren vier Teilmengen
gem_vor <- subset(vorrömisch, morph_Typ_Sprache == "gem")
sla_vor <- subset(vorrömisch, morph_Typ_Sprache == "sla")
gem_lat <- subset(lateinisch, morph_Typ_Sprache == "gem")
sla_lat <- subset(lateinisch, morph_Typ_Sprache == "sla")

#Plotte inschriften.csv mit ggmap
library("ggmap")
coords_insch <- get_coords(inschriften$Geodaten)

inschriften$lat = coords_insch$lat
inschriften$lng = coords_insch$lng
coordinates(inschriften) <- ~ lng + lat

df_insch <- as.data.frame.matrix(inschriften@coords)

insch <- ggmap(get_stamenmap(bbox = c(left = 1, bottom = 42, 
                                  right = 20, top = 50), zoom = 7, maptype = "toner"))
insch + geom_point(aes(x = lng , y = lat), colour = "blue", data = df_insch, alpha = 0.35)

##Plotte die Teilmengen nach den Koordinaten
#gem_vor
library("ggmap")
coords1 <- get_coords(gem_vor$Georeferenz)

gem_vor$lat = coords1$lat
gem_vor$lng = coords1$lng
coordinates(gem_vor) <- ~ lng + lat

df <- as.data.frame.matrix(gem_vor@coords)

gv <- ggmap(get_stamenmap(bbox = c(left = 5, bottom = 43.4, 
                                  right = 18, top = 49.5), zoom = 7, maptype = "toner"))
gv <- gv + geom_point(aes(x = lng , y = lat), colour = "red", data = df, alpha = 0.35)
gv + geom_point(aes(x = lng , y = lat), colour = "blue", data = df_insch, alpha = 0.35)

#gem_lat
coords2 <- get_coords(gem_lat$Georeferenz)

gem_lat$lat = coords2$lat
gem_lat$lng = coords2$lng
coordinates(gem_lat) <- ~ lng + lat

df_gl <- as.data.frame.matrix(gem_lat@coords)

gl <- ggmap(get_stamenmap(bbox = c(left = 5, bottom = 43.4, 
                                   right = 18, top = 49.5), zoom = 7, maptype = "toner"))
gl <- gl + geom_point(aes(x = lng , y = lat), colour = "red", data = df, alpha = 0.35)
gl + geom_point(aes(x = lng , y = lat), colour = "blue", data = df_gl, alpha = 0.35)

