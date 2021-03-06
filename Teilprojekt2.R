z_ling <- read.csv("z_ling.csv", encoding = "UTF-8")
#Duplikate der Sprachbelege entfernen
z_ling2 <- z_ling[!duplicated(z_ling$Id_Instance),]


##Teilmengen bilden (romanisch, rom_lat, rom_vor)
rom <- subset(z_ling2, Type_Lang == "roa")

rom_lat <- subset(rom, Base_Type_Lang == "lat" )
rom_vor <- subset(rom, Base_Type_Lang == "vor")

# Belege, wo die Zuordnung der Basistypen unsicher sind
rom_lat_unsure <- subset(rom_lat, Base_Type_Unsure == 1)
# 56 Beobachtungen
rom_vor_unsure <- subset(rom_vor, Base_Type_Unsure == 1)
# 54 Beobachtungen

#sichere Zuordnung
rom_lat_sure <- subset(rom_lat, Base_Type_Unsure == 0)
rom_vor_sure <- subset(rom_vor, Base_Type_Unsure == 0)

# 'Alpine_Convention' gibt an, ob sich der Beleg in der Alpenkonvention befindet (1) oder nicht (0)
# d.h. 
rom_lat_alp <- subset(rom_lat_sure, Alpine_Convention == 1) # Beobachtungen im Alpenraum
rom_vor_alp <- subset(rom_vor_sure, Alpine_Convention == 1) # Beobachtungen im Alpenraum

# Betrachte, welche Basistypen es überhaupt gibt
table(rom$Base_Type_Lang)

#  ahd   deu   eng   fas   fra   frk   gal   gem   goh   got   grc   lat   lts 
#    6    16     2    37     5   108    73  1503    61    83   170 19733     5 
# NULL   roa   sla   vor   xxx 
# 4430     2     2  1508   481 

##Teilmengen bilden (germanisch, ger_lat, ger_vor)
ger <- subset(z_ling2, Type_Lang == "gem")

ger_lat <- subset(ger, Base_Type_Lang == "lat")
ger_vor <- subset(ger, Base_Type_Lang == "vor")

ger_lat_vor <- subset(ger, c(Base_Type_Lang == "lat", Base_Type_Lang == "vor"))
ger_lat_vor_alp <- subset(ger_lat_vor, Alpine_Convention == 1)

# Belege, wo die Zuordnung der Basistypen unsicher sind
ger_lat_unsure <- subset(ger_lat, Base_Type_Unsure == 1)
ger_vor_unsure <- subset(ger_vor, Base_Type_Unsure == 1)
# jeweils 0 Beobachtungen -> unsure-Beobachtungen existieren nicht


#Welche Basistypen gibt es überhaupt?
table(ger$Base_Type_Lang)

#   ahd   deu   eng   fas   fra   frk   gal   gem   goh   got   grc   lat   lts 
#   296    27     6     0    10     0    92  9173  3361   159    47  5289     0 
#  NULL   roa   sla   vor   xxx 
# 31137     0    12  1880    32 


## Teilmengenbilden (slawisch, slaw_lat, slaw_vor)
slaw <- subset(z_ling2, Type_Lang == "sla")

slaw_lat <- subset(slaw, Base_Type_Lang == "lat")
slaw_vor <- subset(slaw, Base_Type_Lang == "vor")


slaw_vor_lat <- subset(slaw, c(Base_Type_Lang == "lat", Base_Type_Lang == "vor"))
slaw_vor_lat_alp <- subset(slaw_vor_lat, Alpine_Convention == 1)

# Belege, wo die Zuordnung der Basistypen unsicher sind
slaw_lat_unsure <- subset(slaw_lat, Base_Type_Unsure == 1)
slaw_vor_unsure <- subset(slaw_vor, Base_Type_Unsure == 1)
# jeweils 0 Beobachtungen -> unsure-Beobachtungen existieren nicht

#Wie viele Basistypen gibt es überhaupt?
table(slaw$Base_Type_Lang)

# ahd  deu  eng  fas  fra  frk  gal  gem  goh  got  grc  lat  lts NULL  roa  sla 
#  11   58    3    0    0    1    0  825  429    0    0 1157    0  764    0 2793 
# vor  xxx 
# 136    8 


## Erstellen des gesamten Alpenraums als ein Polygon
source("Teilmengen_z_ling.R")
library("dplyr")
library("raster")
library("sp")
library("mapview")
library("ggmap")
one <- readRDS("listone.RDS")
two <- readRDS("listtwo.RDS")
tre <- readRDS("listtre.RDS")
fou <- readRDS("listfor.RDS")
fiv <- readRDS("listfiv.RDS")
six <- readRDS("listsix.RDS")
sev <- readRDS("listsev.RDS")

sp_crowd <- readRDS(file = "sp_crowd.RDS")
box <- sp_crowd@bbox
box[2,] <- c(44, 49)
box[,1] <- c(4.5, 43.3)
box[,2] <- c(16.5, 48.5)
map <- get_stamenmap(bbox = box, zoom = 6, maptype = "toner-lite") 
canvas <- ggmap(map)

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
library("rgeos")
agg <- raster::aggregate(wot)

plot(agg)

# alles in funktionen packen

areas <- list(one, two, tre, fou, fiv, six, sev)
# leeres polygon fuer rbind
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


### Romanische Sprachfamilie 

## Romanisch mit Basistyp 'Lateinisch'

# Überprüfen ob die Belege im Alpenraum sind (eigenlich unnötig, 
# da wir zuvor eine Teilmenge von z_ling gemacht haben mit Berücksichtigung 
# der Alpenkonvention)
# aber als Sicherheit trz nicht ein unbedingt unnötiger Schritt

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

pnts_rom_lat <- get_coords(rom_lat_alp$Geo_Data)

points_rom_lat <- data.frame(lat = as.numeric(pnts_rom_lat$lat), 
                             lng = as.numeric(pnts_rom_lat$lng))

coordinates(points_rom_lat) <- ~ lng + lat
crs(points_rom_lat) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

l8r_rom_lat <- sp::over(points_rom_lat, gates)

# table(l8r)

l8r_g8r_rom_lat <- points_rom_lat[!is.na(over(points_rom_lat, gates)),]
nrow(l8r_g8r_rom_lat@coords)

plot(gates)
plot(l8r_g8r_rom_lat, add = TRUE)
# Ergebnis: Belege liegen im Alpenraum (wie erwartet)

df_rom_lat <- as.data.frame(table(rom_lat_alp$Geo_Data))
df_rom_lat <- subset(df_rom_lat, df_rom_lat$Freq!= 0)
coords_romlat <- get_coords(df_rom_lat$Var1)
df_rom_lat$lng <- coords_romlat$lng
df_rom_lat$lat <- coords_romlat$lat
sp_rom_lat <- df_rom_lat

map_rom <- get_stamenmap(bbox = c(left = 4, bottom = 42, 
                                         right = 18, top = 52), zoom = 6, 
                         maptype = "toner-lite") 

plot_rom_lat <- ggmap(map_rom) + 
  stat_bin2d(mapping = aes(x = lng, y = lat), data = sp_rom_lat , 
             bins = 50, alpha = 0.7) +
  scale_fill_gradient("Anzahl der Belege", low = "lightsalmon", high = "red") +
  ggtitle("Romanisch mit Basistyp 'Lateinisch'")
plot_rom_lat

ggsave("Romanisch_Lateinisch.png", plot = plot_rom_lat, width = 16, height = 12, units = "cm")

## Romanisch mit Basistyp 'Vorrömisch'
pnts_rom_vor <- get_coords(rom_vor_alp$Geo_Data)

points_rom_vor <- data.frame(lat = as.numeric(pnts_rom_vor$lat), 
                             lng = as.numeric(pnts_rom_vor$lng))



coordinates(points_rom_vor) <- ~ lng + lat
crs(points_rom_vor) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

l8r_rom_vor <- sp::over(points_rom_vor, gates)

# table(l8r)

l8r_g8r_rom_vor <- points_rom_vor[!is.na(over(points_rom_vor, gates)),]
nrow(l8r_g8r_rom_vor@coords)

plot(gates)
plot(l8r_g8r_rom_vor, add = TRUE)
# Belege liegen im Alpenraum

df_rom_vor <- as.data.frame(table(rom_vor_alp$Geo_Data))
df_rom_vor <- subset(df_rom_vor, df_rom_vor$Freq != 0)
coords_romvor <- get_coords(df_rom_vor$Var1)
df_rom_vor$lng <- coords_romvor$lng
df_rom_vor$lat <- coords_romvor$lat
sp_rom_vor <- df_rom_vor

map_vor <- get_stamenmap(bbox = c(left = 4, bottom = 42, 
                                  right = 18, top = 52), zoom = 6, 
                         maptype = "toner-lite") 

plot_rom_vor <- ggmap(map_vor) + 
  stat_bin2d(mapping = aes(x = lng, y = lat), data = sp_rom_vor , bins = 50, 
             alpha = 0.7) + 
  scale_fill_gradient("Anzahl der Belege", low = "lightsalmon", high = "red") +
  ggtitle("Romanisch mit Basistyp 'Vorrömisch'")
plot_rom_vor 
ggsave("Romanisch_Vorrömisch.png", plot = plot_rom_vor, width = 16, 
       height = 12, units = "cm")

### Germanisch mit Basistypen 'Vorrömisch' und 'Lateinisch'

df_germ <- as.data.frame(table(ger_lat_vor_alp$Geo_Data))
df_germ <- subset(df_germ, df_germ$Freq != 0)
coords_germ <- get_coords(df_germ$Var1)


df_germ$lng <- coords_germ$lng
df_germ$lat <- coords_germ$lat
sp_germ <- df_germ

map_germ <- get_stamenmap(bbox = c(left = 4, bottom = 42, 
                                  right = 18, top = 52), zoom = 6, 
                          maptype = "toner-lite") 

plot_germ <- ggmap(map_germ) + 
  stat_bin2d(mapping = aes(x = lng, y = lat), data = sp_germ , bins = 50, 
             alpha = 0.8) +
  scale_fill_gradient("Anzahl der Belege", low = "lightsalmon", high = "red" ) +
  ggtitle("Germanisch mit den Basistypen 'Vorrömisch' und 'Lateinisch'")

plot_germ
ggsave("Germanisch_Basistypen.png", plot = plot_germ, width = 16, 
       height = 12, units = "cm")

### Slawisch mit Basistypen 'Vorrömisch' und 'Lateinisch'

df_slaw <- as.data.frame(table(slaw_vor_lat_alp$Geo_Data))
df_slaw <-subset(df_slaw, df_slaw$Freq != 0)
coords_slaw <- get_coords(df_slaw$Var1)
df_slaw$lng <- coords_slaw$lng
df_slaw$lat <- coords_slaw$lat
sp_slaw <- df_slaw

map_slaw <- get_stamenmap(bbox = c(left = 4, bottom = 42, 
                                   right = 18, top = 52), zoom = 6, 
                          maptype = "toner-lite") 
plot_slaw <- ggmap(map_slaw) + 
  stat_bin2d(mapping = aes(x = lng, y = lat), data = sp_slaw , bins = 50, 
             alpha = 0.8) +
  scale_fill_gradient("Anzahl der Belege", low = "lightsalmon", high = "red") +
  ggtitle("Slavisch mit den Basistypen 'Vorrömisch' und 'Lateinisch'")
plot_slaw

ggsave("Slavisch_Basistypen.png", plot = plot_slaw, width = 16, 
       height = 12, units = "cm")
# 26.603 Belege mit Basistypen Lateinisch und Vorrömisch
