# Geodaten Spalte in Länge und Breite einteilen
require(sf)
require(raster)
require(sp)
require(mapview)

coords_of_nicht_rom <- nicht_rom$Georeferenz
coords_of_nicht_rom <- as.character(coords_of_nicht_rom)

Spalte_geo <- sub("POINT", "", coords_of_nicht_rom)
Spalte_geo <- sub("\\(", "", Spalte_geo)
Spalte_geo <- sub("\\)", "", Spalte_geo)


aufgetrennt_nicht_rom <- stringr::str_split(Spalte_geo, " ")
erste_Spalte <- sapply(aufgetrennt_nicht_rom, `[[`, 1)
zweite_Spalte <- sapply(aufgetrennt_nicht_rom, `[[`, 2)

Breite_nicht_rom <- zweite_Spalte
Laenge_nicht_rom <- erste_Spalte


#Map für nicht-rom erstelllen

points_nicht_rom <- data.frame(Breite_nicht_rom = as.numeric(Breite_nicht_rom), Laenge_nicht_rom = as.numeric(Laenge_nicht_rom))
coordinates(points_nicht_rom) <- ~ Laenge_nicht_rom + Breite_nicht_rom
crs(points_nicht_rom) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

counts_nicht_rom <- as.data.frame(table(coords_of_nicht_rom))
Spalte_geo <- sub("POINT\\(", "", counts_nicht_rom$coords_of_nicht_rom)
Spalte_geo <- sub("\\)", "", Spalte_geo)
aufgetrennt_nicht_rom <- stringr::str_split(Spalte_geo, " ")
# add the coords to the counts data frame
counts_nicht_rom$Breite_nicht_rom <- sapply(aufgetrennt_nicht_rom, `[[`, 2)
counts_nicht_rom$Laenge_nicht_rom <- sapply(aufgetrennt_nicht_rom, `[[`, 1)
counts_nicht_rom$Breite_nicht_rom <- as.numeric(counts_nicht_rom$Breite_nicht_rom)
counts_nicht_rom$Laenge_nicht_rom <- as.numeric(counts_nicht_rom$Laenge_nicht_rom)

coordinates(counts_nicht_rom) <- ~ Laenge_nicht_rom + Breite_nicht_rom
crs(counts_nicht_rom) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mapview(counts_nicht_rom)

require(ggmap)

