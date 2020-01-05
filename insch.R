# inschriften: step 1: Betrachten der Daten
# dieses file ist nur als ueberblick gedacht. 
tab <- read.csv("tabula.csv", encoding="UTF-8")
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

inschriften <- read.csv("inschriften.csv", encoding = "UTF-8")
head(inschriften)

insch_coords <- get_coords(inschriften$Geodaten)

insch_crds <- data.frame(lng = insch_coords[[1]], lat = insch_coords[[2]])
names(insch_crds) <- c("lng", "lat")
head(insch_crds)
coordinates(insch_crds) <- ~ lng + lat
plot(insch_crds)
plot(crds, col = "red", add = TRUE)
