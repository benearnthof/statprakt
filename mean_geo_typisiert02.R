#typisiert2 Mittelwert erechnen von Länge und Breite
require(sf)
require(raster)
require(sp)
require(mapview)

coords_of_typisiert2 <- typisiert2$Georeferenz
coords_of_typisiert2 <- as.character(coords_of_typisiert2)

bearbeitet <- sub("POINT", "", coords_of_typisiert2)
bearbeitet <- sub("\\(", "", bearbeitet)
bearbeitet <- sub("\\)", "", bearbeitet)


aufgetrennt <- stringr::str_split(bearbeitet, " ")
eins <- sapply(aufgetrennt, `[[`, 1)
zwei <- sapply(aufgetrennt, `[[`, 2)

Breite_typisisiert2 <- zwei
Laenge_typisiert2 <- eins
Breite_typisisiert2 <- as.numeric(Breite_typisisiert2)
Laenge_typisiert2 <- as.numeric(Laenge_typisiert2)
mean(Breite_typisisiert2)
mean(Laenge_typisiert2)
#Mittelpunkt typisiert2: 11.28436x47.21051

