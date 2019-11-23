# Plot über Daten aus nicht_rom und Inschriften

#Inschriften: schwarz Punkte
insch_coords <- get_coords(inschriften$Geodaten)

insch_crds <- data.frame(lng = insch_coords[[1]], lat = insch_coords[[2]])
names(insch_crds) <- c("lng", "lat")
head(insch_crds)
coordinates(insch_crds) <- ~ lng + lat
plot(insch_crds) # dauert sehr lang => einzigartige koordinatenpaare rausfiltern
insch_crds@coords <- unique(insch_crds@coords)
plot(insch_crds)


# alle Eintragungen aus nicht_rom in Rot
latvor <- nicht_rom[nicht_rom$Sprache_Basistyp %in% c("lat", "vor"),]
latvor_coords <- get_coords(latvor$Georeferenz)
latvor_coords <- cbind(latvor_coords[[1]], latvor_coords[[2]])
latvor_coords <- as.data.frame.matrix(latvor_coords)
latvor_coords <- unique(latvor_coords)
names(latvor_coords) <- c("lng", "lat")

coordinates(latvor_coords) <-  ~ lng + lat
plot(latvor_coords, add = T, col = "red")



# Einträge ger_vor = blau

vorrömisch <- subset(nicht_rom, Sprache_Basistyp == "vor")
gem_vor <- subset(vorrömisch, morph_Typ_Sprache == "gem")

ger_vor_coords <- get_coords(gem_vor$Georeferenz)
ger_vor_coords <- cbind(ger_vor_coords[[1]], ger_vor_coords[[2]])
ger_vor_coords <- as.data.frame.matrix(ger_vor_coords)
ger_vor_coords <- unique(ger_vor_coords)
names(ger_vor_coords) <- c("lng", "lat")

coordinates(ger_vor_coords) <-  ~ lng + lat
plot(ger_vor_coords, add = T, col = "blue")

# Einträge gem_lat in Weiß
lateinisch <- subset(nicht_rom, Sprache_Basistyp == "lat")
gem_lat <- subset(lateinisch, morph_Typ_Sprache == "gem")

gem_lat_coords <- get_coords(gem_lat$Georeferenz)
gem_lat_coords <- cbind(gem_lat_coords[[1]],gem_lat_coords[[2]])
gem_lat_coords <- as.data.frame.matrix(gem_lat_coords)
gem_lat_coords <- unique(gem_lat_coords)
names(gem_lat_coords) <- c("lng", "lat")

coordinates(gem_lat_coords) <-  ~ lng + lat
plot(gem_lat_coords, add = T, col = "white")

#Einträge sla_vor in grün
vorrömisch <- subset(nicht_rom, Sprache_Basistyp == "vor")
sla_vor <- subset(vorrömisch, morph_Typ_Sprache == "sla")


sla_vor_coords <- get_coords(sla_vor$Georeferenz)
sla_vor_coords <- cbind(sla_vor_coords[[1]], sla_vor_coords[[2]])
sla_vor_coords <- as.data.frame.matrix(sla_vor_coords)
sla_vor_coords <- unique(sla_vor_coords)
names(sla_vor_coords) <- c("lng", "lat")

coordinates(sla_vor_coords) <-  ~ lng + lat
plot(sla_vor_coords, add = T, col = "green")

#Einträge sla_lat in darkgoldenrod
lateinisch <- subset(nicht_rom, Sprache_Basistyp == "lat")
sla_lat <- subset(lateinisch, morph_Typ_Sprache == "sla")

sla_lat_coords <- get_coords(sla_lat$Georeferenz)
sla_lat_coords <- cbind(sla_lat_coords[[1]],sla_lat_coords[[2]])
sla_lat_coords <- as.data.frame.matrix(sla_lat_coords)
sla_lat_coords <- unique(sla_lat_coords)
names(sla_lat_coords) <- c("lng", "lat")

coordinates(sla_lat_coords) <-  ~ lng + lat
plot(sla_lat_coords, add = T, col = "darkgoldenrod")



