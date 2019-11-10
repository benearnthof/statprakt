require(sf)
require(raster)
require(sp)
require(mapview)
coords <- crowd01$Georeferenz
coords <- as.character(coords)

geo <- sub("POINT", "", coords)
geo <- sub("\\(", "", geo)
geo <- sub("\\)", "", geo)

list2 <- stringr::str_split(geo, " ")
one <- sapply(list2, `[[`, 1)
two <- sapply(list2, `[[`, 2)

Breite_crowd01 <- two
Länge_crowd01 <- one

Breite_crowd01 <- as.numeric(lat2)

Länge_crowd01 <- as.numeric(lng2)

## Kenngrößen 
mean(Breite_crowd01)
#47.18558
mean(Länge_crowd01)
#11.11622
#Mittelpunkt(11.11622 47.18558)

##Streuung
var(Breite_crowd01)
#0.970931
var(Länge_crowd01)
#3.41473

sd(Breite_crowd01)
#0.9853583
sd(Länge_crowd01)
#1.847899

range(Breite_crowd01)
#37.67845 50.38567
range(Länge_crowd01)
#5.397995 16.268321

## Kenngrößen Zusammenfassung
summary(Länge_crowd01)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.398   9.870  11.618  11.116  12.313  16.268 

summary(Breite_crowd01)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#37.68   46.47   47.15   47.19   47.84   50.39 