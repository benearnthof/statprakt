# descriptive analysis of the crowdsourcing data
crowd <- read.csv("~/statprakt/crowd01.csv", encoding = "UTF-8", sep = ",")
library(ggplot2)
plot(crowd$Kategorie)
ggplot(data = crowd, mapping = aes(x = Kategorie)) +
  geom_bar()

crowd$Erfasst_Am[1]
test <- as.POSIXct(crowd$Erfasst_Am[1])
test <- as.POSIXct(crowd$Erfasst_Am)

index <- seq_along(test)
plot(index ~ test)

# http://personality-project.org/r/r.plottingdates.html

ggplot(crowd,
       aes(x = reorder(Kategorie,Kategorie,
                     function(x)-length(x)))) +
  geom_bar()

typisiert <- read.csv("~/statprakt/typisiert.csv", encoding = "UTF-8")
crowdtyp <- typisiert[typisiert$Crowd == 1,]
head(crowdtyp)
nrow(crowdtyp)
nrow(crowd)

summary(crowdtyp$Id_kategorie)
summary(crowdtyp$Id_Konzept)
hist(crowdtyp$Id_kategorie, breaks = 269)
tail(sort(crowdtyp$Id_kategorie))
lastkat <- crowdtyp[crowdtyp$Id_kategorie == 269,]
head(lastkat)
kategorytable <- sort(table(crowdtyp$Id_kategorie))
top20kats <- tail(kategorytable, n = 20)
top20kats <- attributes(top20kats)$dimnames[[1]]
top20kats <- as.numeric(top20kats)

top20 <- crowdtyp[crowdtyp$Id_kategorie %in% top20kats,]
hist(top20$Id_kategorie, breaks = 20)

# okay lets take a look at the crowdsourcing data. 
# columns erfasst am and Id_Informant contain valuable data. 
require(magrittr)
grouped <- crowd %>% dplyr::group_by(Id_Informant)

require(stringr)
days <- stringr::str_split(string = crowd$Erfasst_Am, pattern = " ")
days <- sapply(days, `[[`, 1)
plot(table(days))
table <- as.data.frame(table(days))
hist(table(days))
require(ggplot2)

ggplot(table, aes(x = days, y = Freq)) + geom_bar(stat = "identity") +
  geom_vline(xintercept = 221, col = "red", alpha = 0.5)

# we can match days to add vertical lines at the corresponding publicity event days

  # 2001 seems to be an outlier
pubdays <- publicity$Datum[-1]
pubdays <- as.Date(pubdays)
?match
# these are the positions we want to add vertical lines at in the plot
index <- match(pubdays, table$days)
# there are a few NA entries because no corresponding match was found. 
pubdays[is.na(index)]
# 10 days to be exact. If we fill in the days table with 0 entries on days with 
# no added entries we should be able to match without NA entries. 

# lets generate a vector of days from the start to the end
start <- as.Date(min(days))
end <- as.Date(max(days))
df <- data.frame(days = seq(from = start, to = end, by = 1))
df$freq <- 0
days <- as.Date(days)
table$days <- as.Date(table$days)
index <- match(table$days, df$days)
df$freq[index] <- table$Freq

ggplot(df, aes(x = days, y = freq)) + geom_bar(stat = "identity") +
  geom_vline(xintercept = as.Date("2018-05-02"), col = "red")
# we can add vertical lines indeed!
# add all the lines for all publicity events

plot <- ggplot(df, aes(x = days, y = freq)) + geom_bar(stat = "identity")

for (i in seq_along(pubdays)) {
  plot <- plot + 
  geom_vline(xintercept = as.Date(pubdays[i]), col = "blue", alpha = 0.5)
}
plot

ende <- df[(nrow(df) - 120):nrow(df),]
plot_ende <-  ggplot(ende, aes(x = days, y = freq)) + geom_bar(stat = "identity")
for (i in seq_along(pubdays)) {
  plot_ende <- plot_ende + 
    geom_vline(xintercept = as.Date(pubdays[i]), col = "blue", alpha = 0.5)
}
plot_ende



x <- df$freq
x2 <- data.table::shift(x, n = -1L)
x <- x[-length(x)]
x2 <- x2[-length(x2)]
cor(x, x2)
# better way to calc autocorrelation: stats::acf
stats::acf(df$freq, lag.max = 20, type = "correlation")

informanten <- unique(crowd$Id_Informant)
length(informanten)
nrow(crowd)/length(informanten)

require(tidyverse)
idtable_orig <- crowd %>% select(Id_Informant) %>% group_by(Id_Informant) %>% table()
plot(idtable_orig)
# rename the dimnames to 1:1043
idtable <- sort(idtable_orig, decreasing = TRUE)
dimnames(idtable) <- list(as.character(c(1:length(dimnames(idtable)[[1]]))))
plot(idtable)

plot(ecdf(idtable))
ecdf_idtable <- ecdf(idtable)
v <- as.vector(idtable)
ecdf_v <- ecdf(v)
tab <- table(idtable)
plot(ecdf(tab))
df <- as.data.frame(tab)
names(df) <- c("Einträge", "Häufigkeit")
plot(ecdf(df$Häufigkeit))

# we're interested in the amount of people that contributed x percent

head(df)
df$Einträge <- as.numeric(levels(df$Einträge))[df$Einträge]
product <- df[,1] * df[,2]
df$Product <- product
df$Anteil <- df$Product / sum(df$Product)
df$Kumuliert <- cumsum(df$Anteil)
plot(df$Einträge~df$Kumuliert)
plot(df$Kumuliert~df$Einträge)
df$Anti <- 1 - df$Kumuliert
plot(df$Anti~df$Einträge)
df$id <- 1:95
plot(df$Kumuliert ~ df$id)
df$Personenanteil <- df$Häufigkeit / sum(df$Häufigkeit)
df$Personenkumuliert <- cumsum(df$Personenanteil)
plot(df$Personenkumuliert ~ df$Kumuliert, ylim = c(0,1))
abline(h = 0.95, col = "red")
which(1 - df$Personenkumuliert < 0.05)
df$Kumuliert[53]
abline(v = df$Kumuliert[53], col = "red")


# Hauptkategorieplot
require(reshape2)
test <- melt(crowd$Hauptkategorie)
mlt <- melt(table(test))
mlt <- mlt[order(mlt$value, decreasing = TRUE),]
ggplot(mlt, aes(x = reorder(test, -value), y = value)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label=value), vjust = -0.1, col = "black")

# lets visualize the locations of crowdfunding now
require(mapview)
require(raster)
require(sf)
require(stringr)
head(crowd)

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

coords <- get_coords(crowd$Georeferenz)

crowd$lat = coords$lat
crowd$lng = coords$lng

sp_crowd <- crowd
coordinates(sp_crowd) <- ~ lng + lat
crs(sp_crowd) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mapview(sp_crowd, zcol = "Hauptkategorie")

require(ggmap)
x <- sp_crowd@coords
minmax <- c(min(x[,1]), max(x[,1]), min(x[,2]), max(x[,2]))
map <- get_stamenmap(bbox = c(left = 11.6, bottom = 45, 
                              right = 14.5, top = 47), zoom = 8)
ggmap(map)
ggmapplot(map) +
  geom_point(head(x), aes(x = "lng", y = "lat"))


# install.packages("rworldmap")
# install.packages("rworldxtra")
require(rworldxtra)
require(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = sp_crowd@bbox[1,], ylim = sp_crowd@bbox[2,], asp = 0.75)
points(sp_crowd@coords[,1], sp_crowd@coords[,2], col = "red", cex = .6)
abline(v = 6)

# lets count entries per point
df <- as.data.frame(table(crowd$Georeferenz))
df
coords <- get_coords(df$Var1)
df$lng <- coords$lng
df$lat <- coords$lat
head(df)
library(scales)
rescale()
plot(newmap, xlim = sp_crowd@bbox[1,], ylim = sp_crowd@bbox[2,], asp = 0.75)
points(df$lng, df$lat, col = "red", cex = log(df$Freq) + 1)

box <- sp_crowd@bbox
box[2,] <- c(44, 49)
map <- get_stamenmap(bbox = box, zoom = 7, maptype = "toner") 
ggmap(map) +
  geom_point(aes(x = lng, y = lat, size = log(Freq)),
             data = df, alpha = .5, col = "#F00000")
