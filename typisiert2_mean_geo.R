require(sf)
require(raster)
require(sp)

require(mapview)

coords_typisiert <- typisiert2$Georeferenz
coords_typisiert <- as.character(coords_typisiert)
geo_typisiert <- sub("POINT", "", coords_typisiert)
geo_typisiert <- sub("\\(", "", geo_typisiert)
geo_typisiert <- sub("\\)", "", geo_typisiert)

list_typisiert <- stringr::str_split(geo_typisiert, " ")
one_typ <- sapply(list_typisiert, `[[`, 1)
two_typ <- sapply(list_typisiert, `[[`, 2)

Breite_typisiert <- two_typ
Länge_typisiert <- one_typ


Breite_typisiert <- as.numeric(Breite_typisiert)

Länge_typisiert <- as.numeric(Länge_typisiert)

mean(Breite_typisiert)
#47.21051
mean(Länge_typisiert)
#11.28436
#Mittelpunkt (11.28436 47.21051)

##Streuung
#Varianz
var(Länge_typisiert)
#3.669273
var(Breite_typisiert)
#0.364846

sd(Länge_typisiert)
#1.915535
sd(Breite_typisiert)
#0.6040248

##Kenngrößen Zusammenfassung
summary(Länge_typisiert)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.398   9.996  11.899  11.284  12.508  16.268 

summary(Breite_typisiert) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#44.12   46.73   47.23   47.21   47.79   48.29 