# inschriften: step 1: Betrachten der Daten
require(raster)
require(sf)
tab <- read.csv("~/statprakt/tabula.csv", encoding="UTF-8")
head(tab)

library("raster")
library("sp")
x <- tab$Geodaten
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

coords <- get_coords(dta)

length(coords[[1]])

crds <- cbind(coords[[1]], coords[[2]])
crds <- as.data.frame.matrix(crds)
names(crds) <- c("lng", "lat")

coordinates(crds) <-  ~ lng + lat
plot(crds)

inschriften <- read.csv("~/statprakt/inschriften.csv", encoding = "UTF-8")
head(inschriften)

insch_coords <- get_coords(inschriften$Geodaten)

insch_crds <- data.frame(lng = insch_coords[[1]], lat = insch_coords[[2]])
names(insch_crds) <- c("lng", "lat")
head(insch_crds)
coordinates(insch_crds) <- ~ lng + lat
plot(insch_crds) # dauert sehr lang => einzigartige koordinatenpaare rausfiltern
insch_crds@coords <- unique(insch_crds@coords)
plot(insch_crds)
plot(crds, col = "blue", add = TRUE)

head(inschriften)

# Frage: Gibt es eine Häufung romanischer Basistypen in der Nähe von Zentren 
# lateinischer Inschriftenfunde?

fulldta <- read.csv("~/statprakt/z_ling.csv", encoding = "UTF-8")
# kommentar online: Distanzmaß für ähnlichkeit der Begriffe zueinander & Ranking
summary(fulldta$Base_Type_Lang)

# nur eintraege aus lateinischem und vorroemischem typ
latvor <- fulldta[fulldta$Base_Type_Lang %in% c("lat", "vor"),]

head(latvor$Geo_Data)

latvor_coords <- get_coords(latvor$Geo_Data)
latvor_coords <- cbind(latvor_coords[[1]], latvor_coords[[2]])
latvor_coords <- as.data.frame.matrix(latvor_coords)
latvor_coords <- unique(latvor_coords)
names(latvor_coords) <- c("lng", "lat")

coordinates(latvor_coords) <-  ~ lng + lat
plot(latvor_coords, add = T, col = "red")

