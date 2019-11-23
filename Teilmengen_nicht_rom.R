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
insch + geom_point(aes(x = lng , y = lat), colour = "#377eb8", data = df_insch, alpha = 0.35)

###Plotte die Teilmengen nach den Koordinaten

##gem_vor
library("ggmap")
coords1 <- get_coords(gem_vor$Georeferenz)

gem_vor$lat = coords1$lat
gem_vor$lng = coords1$lng
coordinates(gem_vor) <- ~ lng + lat

df <- as.data.frame.matrix(gem_vor@coords)

gv <- ggmap(get_stamenmap(bbox = c(left = 5, bottom = 43.4, 
                                  right = 18, top = 49.5), zoom = 7, maptype = "toner"))
gv <- gv + geom_point(aes(x = lng , y = lat), colour = "#e41a1c", data = df, alpha = 0.35)
gv + geom_point(aes(x = lng , y = lat), colour = "#377eb8", data = df_insch, alpha = 0.35)

#Heranzoomen des entsprechenden Gebiets
gv2 <- ggmap(get_stamenmap(bbox = c(left = 6.5, bottom = 45.2, 
                                   right = 17.5, top = 49.3), zoom = 7, maptype = "toner"))
gv2 <- gv2 + geom_point(aes(x = lng , y = lat), colour = "#e41a1c", data = df, alpha = 0.35)
gv2 + geom_point(aes(x = lng , y = lat), colour = "#377eb8", data = df_insch, alpha = 0.35)

##gem_lat
coords2 <- get_coords(gem_lat$Georeferenz)

gem_lat$lat = coords2$lat
gem_lat$lng = coords2$lng
coordinates(gem_lat) <- ~ lng + lat

df_gl <- as.data.frame.matrix(gem_lat@coords)

gl <- ggmap(get_stamenmap(bbox = c(left = 5, bottom = 43.4, 
                                   right = 18, top = 49.5), zoom = 7, maptype = "toner"))
gl <- gl + geom_point(aes(x = lng , y = lat), colour = "#ff7f00", data = df_gl, alpha = 0.35)
gl + geom_point(aes(x = lng , y = lat), colour = "#377eb8", data = df_insch, alpha = 0.35)

#Heranzoomen des entsprechenden Gebiets
gl2 <- ggmap(get_stamenmap(bbox = c(left = 6.2, bottom = 45.3, 
                                   right = 17.5, top = 49), zoom = 7, maptype = "toner"))
gl2 <- gl2 + geom_point(aes(x = lng , y = lat), colour = "#ff7f00", data = df_gl, alpha = 0.35)
gl2 + geom_point(aes(x = lng , y = lat), colour = "#377eb8", data = df_insch, alpha = 0.35)

##sla_vor
coords3 <- get_coords(sla_vor$Georeferenz)

sla_vor$lat = coords3$lat
sla_vor$lng = coords3$lng
coordinates(sla_vor) <- ~ lng + lat

df_sv <- as.data.frame.matrix(sla_vor@coords)

sv <- ggmap(get_stamenmap(bbox = c(left = 5, bottom = 43.4, 
                                   right = 18, top = 49.5), zoom = 7, maptype = "toner"))
sv <- sv + geom_point(aes(x = lng , y = lat), colour = "#4daf4a", data = df_sv, alpha = 0.35)
sv + geom_point(aes(x = lng , y = lat), colour = "#377eb8", data = df_insch, alpha = 0.35)

#Heranzoomen des entsprechenden Gebiets
sv2 <- ggmap(get_stamenmap(bbox = c(left = 12.5, bottom = 45.8, 
                                    right = 16, top = 47), zoom = 9, maptype = "toner"))
sv2 <- sv2 + geom_point(aes(x = lng , y = lat), colour = "#4daf4a", data = df_sv, alpha = 0.6, size = 2)
sv2 + geom_point(aes(x = lng , y = lat), colour = "#377eb8", data = df_insch, alpha = 0.6, size = 2)

##sla_lat
coords4 <- get_coords(sla_lat$Georeferenz)

sla_lat$lat = coords4$lat
sla_lat$lng = coords4$lng
coordinates(sla_lat) <- ~ lng + lat

df_sl <- as.data.frame.matrix(sla_lat@coords)

sl <- ggmap(get_stamenmap(bbox = c(left = 5, bottom = 43.4, 
                                   right = 18, top = 49.5), zoom = 7, maptype = "toner"))
sl <- sl + geom_point(aes(x = lng , y =
                            lat), colour = "#984ea3", data = df_sl, alpha = 0.35)
sl + geom_point(aes(x = lng , y = lat), colour = "#377eb8", data = df_insch, alpha = 0.35)
#3 Punkte von sla_lat im Süden Frankreichs -> Ausreißer?

#Heranzoomen des entsprechenden Gebiets
sl2 <- ggmap(get_stamenmap(bbox = c(left = 12.6, bottom = 45.4, 
                                   right = 16.5, top = 47), zoom = 7, maptype = "toner"))
sl2 <- sl2 + geom_point(aes(x = lng , y =
                            lat), colour = "#984ea3", data = df_sl, alpha = 0.35)
sl2 + geom_point(aes(x = lng , y = lat), colour = "#377eb8", data = df_insch, alpha = 0.35)