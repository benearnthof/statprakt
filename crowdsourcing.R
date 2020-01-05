# deskriptive analyse der crowdsourcing daten
crowd <- read.csv("crowd01.csv", encoding = "UTF-8", sep = ",")
library(ggplot2)
plot(crowd$Kategorie)
ggplot(data = crowd, mapping = aes(x = Kategorie)) +
  geom_bar()

crowd$Erfasst_Am[1]
# test ob datumseintraege korrekt behandelt werden 
test <- as.POSIXct(crowd$Erfasst_Am[1])
test <- as.POSIXct(crowd$Erfasst_Am)

index <- seq_along(test)
plot(index ~ test)

# http://personality-project.org/r/r.plottingdates.html

# der groesse nach angeordneter plot
ggplot(crowd,
       aes(x = reorder(Kategorie,Kategorie,
                       function(x)-length(x)))) +
  geom_bar()

typisiert <- read.csv("typisiert.csv", encoding = "UTF-8")
crowdtyp <- typisiert[typisiert$Crowd == 1,]
head(crowdtyp)
nrow(crowdtyp)
nrow(crowd)

# die eintraege des typisierten datensatzes sind alle in crowd enthalten
test <- crowdtyp$Georeferenz %in% crowd$Georeferenz
all.equal(sum(test), nrow(crowdtyp))

# ueberblick ueber kategorien die im datensatz enthalten sind
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


# crowdsourcing daten
# spalten erfasst am und Id_Informant enthalten daten von interesse
# magrittr fuer pipe operator 
require(magrittr)
grouped <- crowd %>% dplyr::group_by(Id_Informant)

require(stringr)
# konvertieren von strings in tage
days <- stringr::str_split(string = crowd$Erfasst_Am, pattern = " ")
days <- sapply(days, `[[`, 1)
plot(table(days))
table <- as.data.frame(table(days))
hist(table(days))
require(ggplot2)

ggplot(table, aes(x = days, y = Freq)) + geom_bar(stat = "identity") +
  geom_vline(xintercept = 221, col = "red", alpha = 0.5)

# horizontale linien koennen im plot an gewuensten tagen hinzugefuegt werden

# 2001 scheint ausreisser zu sein
publicity <- read.csv("publicity.csv", encoding = "UTF-8")
pubdays <- publicity$Datum[-1]
pubdays <- as.Date(pubdays)
#?match
# pubdays sind die tage an denen wir farbige markierungen in den plots wollen
index <- match(pubdays, table$days)
# es gibt NA eintraege weil kein match gefunden werden konnte 
pubdays[is.na(index)]

# generiere einen vektor mit tagen von start zu ende
start <- as.Date(min(days))
end <- as.Date(max(days)) # das ende sollte der 31.12. sein um die plots 
# konsistent zu machen
end <- as.Date("2019-12-31")
df <- data.frame(days = seq(from = start, to = end, by = 1))
df$freq <- 0
days <- as.Date(days)
table$days <- as.Date(table$days)
index <- match(table$days, df$days)
df$freq[index] <- table$Freq

ggplot(df, aes(x = days, y = freq)) + geom_bar(stat = "identity") +
  geom_vline(xintercept = as.Date("2018-05-02"), col = "red")

# schritt 1: fuege alle linien fuer alle publicity aktionen hinzu

plot <- ggplot(df, aes(x = days, y = freq)) + geom_bar(stat = "identity")

for (i in seq_along(pubdays)) {
  plot <- plot + 
    geom_vline(xintercept = as.Date(pubdays[i]), col = "blue", alpha = 0.5)
}
plot

# plot fuer eine teilmenge der daten
ende <- df[(nrow(df) - 120):nrow(df),]
plot_ende <-  ggplot(ende, aes(x = days, y = freq)) + geom_bar(stat = "identity")
for (i in seq_along(pubdays)) {
  plot_ende <- plot_ende + 
    geom_vline(xintercept = as.Date(pubdays[i]), col = "blue", alpha = 0.5)
}
plot_ende

# aufteilen der daten in drei jahre
df2017 <- df[(df$days <= "2017-12-31") & (df$days >= "2017-01-01"),]
df2018 <- df[(df$days <= "2018-12-31") & (df$days >= "2018-01-01"),]
df2019 <- df[df$days > "2018-12-31",]

# saveRDS(df2017, "df2017.RDS")
# saveRDS(df2018, "df2018.RDS")
# saveRDS(df2019, "df2019.RDS")

headntail = function(x, ...) {
  h <- head(x, ...)
  t <- tail(x, ...)
  rbind(h, t)
}

lapply(list(df2017, df2018, df2019), headntail, n = 5L)
# scheint zu funktionieren
# aufteilen von pubdays in benoetigtes format
pubdays2017 <- pubdays[pubdays <= "2017-12-31" & pubdays >= "2017-01-01"] 
pubdays2018 <- pubdays[pubdays > "2017-12-31" & pubdays <= "2018-12-31"]
pubdays2019 <- pubdays[pubdays > "2018-12-31"]

# saveRDS(pubdays2017, "pubdays2017.RDS")
# saveRDS(pubdays2018, "pubdays2018.RDS")
# saveRDS(pubdays2019, "pubdays2019.RDS")

pubdays2017 <- readRDS("pubdays2017.RDS")
pubdays2018 <- readRDS("pubdays2018.RDS")
pubdays2019 <- readRDS("pubdays2019.RDS")

# funktion um alles automatisch zu plotten
plt_publicity <- function(df, days, year, size = 2, dayofinterest = NULL) {
  # daysofinterest legt fest welche tage blau eingefaerbt werden sollen
  plt <- ggplot(df, aes(x = days, y = freq)) +
    geom_bar(stat = "identity", col = "black") +
    ylim(0, 600) +
    theme_bw() + 
    theme(axis.text = element_text(size = 18, face = "bold"),
          axis.title = element_text(size = 18, face = "bold")) +
    xlab(year) +
    scale_x_date(date_labels = c("31.12.", "01.01.", "01.04.", "01.07.", "01.10.")) +
    ylab("Anzahl Beiträge")
  # konstruieren von werten fuer geom_segment
  values <- numeric(length = length(days))
  for (i in seq_along(days)) {
    tmp <- df$freq[df$days == as.Date(days[i])]
    values[i] <- if (length(tmp) == 0) {
      0
    } else {tmp}
  }
  segment_data = data.frame(
    x = as.Date(days),
    xend = as.Date(days), 
    y = values,
    yend = rep(600, times = length(days)),
    z = rep(0, times = length(days))
  )
  if (!is.null(dayofinterest)) {
    segment_data$z[segment_data$x %in% as.Date(dayofinterest)] <- 1
  }
  # fuege die segmente dem plot hinzu
  if (!is.null(dayofinterest)) {
    plt <- plt + geom_segment(data = segment_data, 
                              aes(x = x, y = y, xend = xend, yend = yend, color = z), 
                              alpha = 0.75, size = size) + 
      scale_color_gradientn(colours = c("#ff5500","#0050ff")) + 
      theme(legend.position = "none")
  } else {
    plt <- plt + geom_segment(data = segment_data,
                              aes(x = x, y = y, xend = xend, yend = yend),
                              alpha = 0.75, size = size, color = "#ff5500") +
      theme(legend.position = "none")
  }
  plt
}

# daysofinterest sind hier die 3 tage der top 3 publicity aktionen
plot_2017 <- plt_publicity(df2017, pubdays2017, "2017", size = 1.4,
                           dayofinterest = NULL)
plot_2017
# ggsave("Publicity_2017.png", plot = plot_2017, width = 18, height = 12, units = "cm")
plot_2018 <- plt_publicity(df2018, pubdays2018, "2018", size = 1.4,
                           dayofinterest = c("2018-04-27", "2018-06-19"))
plot_2018
# ggsave("Publicity_2018.png", plot = plot_2018, width = 18, height = 12, units = "cm")
plot_2019 <- plt_publicity(df2019, pubdays2019, "2019", size = 1.4,
                           dayofinterest = c("2019-08-08"))
plot_2019
# ggsave("Publicity_2019.png", plot = plot_2019, width = 18, height = 12, units = "cm")


informanten <- unique(crowd$Id_Informant)

length(informanten)
nrow(crowd)/length(informanten)
# jede person fuegte im durchschnitt 13.5 eintraege in die datenbank.
# sind diese gleichmaessig verteilt?

require(tidyverse)
idtable_orig <- crowd %>% select(Id_Informant) %>% group_by(Id_Informant) %>% table()
plot(idtable_orig)
# umbenennen der dimnames zu 1:1043
idtable <- sort(idtable_orig, decreasing = TRUE)
dimnames(idtable) <- list(as.character(c(1:length(dimnames(idtable)[[1]]))))

plot(idtable)
# es scheint deutliche ungleichheiten in der menge der beitraege pro person zu geben

plot(ecdf(idtable))
ecdf_idtable <- ecdf(idtable)
v <- as.vector(idtable)
ecdf_v <- ecdf(v)
tab <- table(idtable)
plot(ecdf(tab))
df <- as.data.frame(tab)
names(df) <- c("Einträge", "Häufigkeit")
plot(ecdf(df$Häufigkeit))

# wir interessieren uns fuer den prozentsatz von personen die eine gewisse 
# prozentpunktemenge an beitraegen geliefert haben

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
which(1 - df$Personenkumuliert < 0.01)
df$Kumuliert[85]
abline(v = df$Kumuliert[53], col = "red")

# install.packages("ineq")
# lorenzkurven shenanigans
library(ineq)
x <- runif(20)

wot <- Lc(df$Personenanteil, plot = TRUE)
abline(h = 1 - df$Kumuliert[53], col = "red")
which(1 - df$Personenkumuliert < 0.05)
df$Kumuliert[53]
abline(v = 0.95, col = "red")
# benutze wot um ggplot für lorenzkurve zu erstellen
plot(wot$L~wot$p)
lorenz <- data.frame(x = wot$p, y = wot$L)
# Empirische verteilungsfunktion ====
empi <- ggplot(data = df, aes(x = Kumuliert, y = Personenkumuliert)) +
  geom_step(size = 1.5) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, round(df$Kumuliert[53], digits = 2), 
                                                  0.75, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 0.95, 1)) +
  theme_bw() +
  geom_hline(yintercept = 0.95, color = "red") +
  geom_vline(xintercept = round(df$Kumuliert[53], digits = 2), color = "red") +
  ggtitle("Empirische Verteilungsfunktion: Crowdsourcing") +
  ylab("Kumulierte relative Anteile: Personen") +
  xlab("Kumulierte relative Anteile: Einträge")

# ggsave("crowdsource_ecdf.png", plot = empi, width = 18, height = 12, units = "cm")  

# Hauptkategorieplot
require(reshape2)
test <- melt(crowd$Hauptkategorie)
mlt <- melt(table(test))
mlt <- mlt[order(mlt$value, decreasing = TRUE),]
ggplot(mlt, aes(x = reorder(test, -value), y = value)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label=value), vjust = -0.1, col = "black")

# visualisierung der orte des crowdsourcings und der orte der beitraege
require(mapview)
require(raster)
require(sf)
require(stringr)
head(crowd)

# funktion um georeferenzeintraege in koordinatenpaare umzuwandeln
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

# umwandeln in spatial daten
sp_crowd <- crowd
coordinates(sp_crowd) <- ~ lng + lat
crs(sp_crowd) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

require(ggmap)
x <- sp_crowd@coords
minmax <- c(min(x[,1]), max(x[,1]), min(x[,2]), max(x[,2]))
map <- get_stamenmap(bbox = c(left = 5.5, bottom = 44.8, 
                              right = 17, top = 47), zoom = 8)
ggmap(map)

# install.packages("rworldmap")
# install.packages("rworldxtra")
require(rworldxtra)
require(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = sp_crowd@bbox[1,], ylim = sp_crowd@bbox[2,], asp = 0.75)
points(sp_crowd@coords[,1], sp_crowd@coords[,2], col = "red", cex = .6)
# punkte in sizilien sind fuer analyse im alpenraum nicht interessant
abline(v = 6)


## Heatmap mit stat_bin2d
df <- as.data.frame(table(crowd$Georeferenz))
df
coords <- get_coords(df$Var1)
df$lng <- coords$lng
df$lat <- coords$lat
head(df)
saveRDS(df, file = "lastone.RDS")
library(scales)
rescale()
plot(newmap, xlim = sp_crowd@bbox[1,], ylim = sp_crowd@bbox[2,], asp = 0.75)
points(df$lng, df$lat, col = "red", cex = log(df$Freq) + 1)

saveRDS(sp_crowd, file = "sp_crowd.RDS")
box <- sp_crowd@bbox
box[2,] <- c(45, 50.5)
map <- get_stamenmap(bbox = c(left = 4, bottom = 43.4, 
                              right = 17.1, top = 51), zoom = 6, maptype = "toner-lite") 
plt <- ggmap(map) +
  geom_point(aes(x = lng, y = lat, size = sqrt(Freq), col = sqrt(Freq)),
             data = df, alpha = .5)
plt
obj <- as.data.frame.matrix(sp_crowd@coords)
plt2 <- ggmap(map) + 
  stat_bin2d(mapping = aes(x = lng, y = lat), data = obj, bins = 30, alpha = 0.7) +
  scale_fill_gradient("Anzahl der Einträge", low ="darkseagreen1", high = "darkseagreen4", limits = c(0, 800))
plt2

# versuch mit stat bin2d
obj$class <- obj$lng* obj$lat
plt3 <- ggmap(map) + 
  stat_bin2d(mapping = aes(x = lng, y = lat, group = class), data = obj, bins = 30, alpha = 0.7)
plt3

library("dplyr")
# hinzufuegen der sprachgebiete als polygone
romanisch <- readRDS("listone.RDS")
df_rom <- as.data.frame.matrix(romanisch[[1]]@coords)
df_rom <- distinct(df_rom)
plot3 <- plt2 +
  geom_polygon(data = df_rom, aes( x = lng, y = lat, colour = "Romanisch"), fill = NA, size = 1) 

germanisch <- readRDS("listtwo.RDS")
df_ger <- as.data.frame.matrix(germanisch[[1]]@coords)
df_ger <- distinct(df_ger)

plot4 <- plot3 +
  geom_polygon(data = df_ger, aes(x = lng, y = lat, colour = "Germanisch"), fill = NA, size =1) +
  ggtitle("Crowdsourcing: Räumliche Verteilung")
slavisch <- readRDS("listtre.RDS")

df_slav <- as.data.frame.matrix(slavisch[[1]]@coords)
df_slav <- distinct(df_slav)
plot5 <- plot4 + 
  geom_polygon(data = df_slav, aes(x = lng, y = lat, colour = "Slavisch"), fill = NA, size = 1) +
  scale_colour_manual("Sprachgebiete",values = c(Germanisch ="blue", Romanisch = "red", Slavisch = "gold")) +
  scale_fill_gradient("Anzahl der Einträge", low ="darkseagreen1", high = "darkseagreen4", limits = c(0, 800))
plot5

# ggsave("Crowdsourcing_raeumlich.png", plot = plot5, width = 16, height = 12, units = "cm")



# es scheint einen peak bei Rosenheim zu geben. 
# Betrachtet man die 10 Tage nach dem Artikel auf Rosenheim24.de ergibt sich:
rosenheim <- df2018[df2018$days >= "2018-06-19" & df2018$days < "2018-06-24",]
sum(rosenheim$freq)
# 679 Einträge innerhalb von 5 (eigentlich 4) Tagen!

crowd$Erfasst_Am <- as.character(crowd$Erfasst_Am)

rosenheim2 <- crowd[crowd$Erfasst_Am >= "2018-06-19" & crowd$Erfasst_Am < "2018-06-24", ]
sort(table(rosenheim2$Id_Informant), decreasing = TRUE)
# nur 2 user mit mehr als 100 einträgen
# lasst uns rausfinden was den poweruser mit 611 einträgen motiviert hat

poweruser <- unlist(dimnames(head(sort(table(crowd$Id_Informant), decreasing = TRUE), n = 10L)))

powerdata <- crowd[crowd$Id_Informant %in% poweruser,]
nrow(powerdata)
userdata1 <- powerdata[powerdata$Id_Informant == poweruser[1],]

# funktion um dates und times aufzuteilen
get_datestimes <- function(x) {
  list <- stringr::str_split(x, " ")
  dates <- as.character(sapply(list, `[[`, 1))
  times <- as.character(sapply(list, `[[`, 2))
  list(dates = dates, times = times)
}
powerdates <- get_datestimes(powerdata$Erfasst_Am)[1]
sort(table(powerdates), decreasing = TRUE)
# tabelle mit daten an denen poweruser (top 10 user) eintraege gemacht haben.

user1dates <- get_datestimes(userdata1$Erfasst_Am)[1]
table(user1dates)
# was passierte am 12.08.?
publicity$Datum <- as.Date(publicity$Datum)
publicity[publicity$Datum >= "2019-08-02" & publicity$Datum < "2019-08-22",]
# Verschiedene Beitr#ge im italienischen internet
# => finde die geolocation dieser beitraege
user1points <- crowd[crowd$Id_Informant == 14858,]
user1points <- user1points$Georeferenz
user1points <- get_coords(user1points)
table(user1points)
# es scheint der gleiche user zu sein, der ort ist für alle 611 eintraege gleich
# points der map hinzufuegen

userpoints <- crowd[crowd$Id_Informant %in% poweruser,]
userpoints$points <- userpoints$Georeferenz
userpoints$lng <- get_coords(userpoints$points)[[1]]
userpoints$lat <- get_coords(userpoints$points)[[2]]


uniquepoints <- userpoints[!duplicated(userpoints$Id_Informant), ]
uniquepoints <- cbind(uniquepoints$Id_Informant, uniquepoints$lng, uniquepoints$lat)

tbl <- table(crowd[crowd$Id_Informant %in% poweruser,]$Id_Informant)
identical(uniquepoints[,1], as.numeric(unlist(dimnames(tbl))))
# die reihenfolge ist identisch
uniquepoints <- as.data.frame(uniquepoints)
uniquepoints$count <- as.vector(tbl)
names(uniquepoints) <- c("ID", "lng", "lat", "count")

# saveRDS(uniquepoints, file = "uniquepoints.RDS")
plt2$labels$fill <- "Anzahl Beiträge"
names(uniquepoints) <- c("ID", "lng", "lat", "Anzahl Einträge der Power User")
plt3 <- plt2 +
  geom_point(aes(x = lng, y = lat, size = `Anzahl Einträge der Power User`), 
             data = uniquepoints, col = "red") +
  ggtitle("Power User")
plt3

# ggsave("Crowdsourcing_Poweruser.png", plot = plt3, width = 16, height = 12, units = "cm")

names(uniquepoints) <- c("ID", "lng", "lat", "count")

# kategorie maps hinzufügen 
# 
# Crowdsourcing: Rangordnung der erfolgreichsten Publicity Aktionen, Evtl gruppieren in Klassen und vergleich dieser. 
# Visualisieren von Publicity Aktionen und impact auf Karten
# Base Activity & Publicity Activity
# Welche Kategorien wurden hauptsächlich beantwortet?

head(crowd)
head(publicity)
which(diff(sort(pubdays)) == 1)
# Auf 6 der 43 Publicityaktionen folgte direkt eine weitere Publicityaktion am 
# naechsten Tag.
start <- as.Date(min(days))
end <- as.Date(max(days))
df <- data.frame(days = seq(from = start, to = end, by = 1))
df$freq <- 0
days <- as.Date(days)
table$days <- as.Date(table$days)
index <- match(table$days, df$days)
df$freq[index] <- table$Freq

rosenheim <- df2018[df2018$days >= "2018-06-19" & df2018$days < "2018-06-24",]

# benutze aehnliches matching fuer alle pubdays
pubresults <- numeric(length = 43L)
for (i in seq_along(pubdays)) {
  pubresults[i] <- df$freq[df$days == sort(pubdays)[i]]
  # mit tag nach publicity aktion
  pubresults[i] <- df$freq[df$days == (sort(pubdays)[i] + 1)]
}

dfpubresults <- data.frame(pubresults = pubresults, dates = sort(pubdays))
dfpubresults$aktion <- publicity$Bericht[2:length(publicity$Bericht)]

top10 <- dfpubresults[order(dfpubresults$pubresults, decreasing = TRUE),][1:10,]
top10$title <- c("Dialekt Themenwoche im BR (27.04.2018)", "Artikel auf rosenheim 24.de (19.06.2018)", "Beitrag auf Facebookseite Servizio Minoranze (09.08.2019)", 
                 "Artikel auf zalp.ch (27.03.2017)", "Artikel auf lmu.de (15.05.2017)", "Bericht auf brennerbasisdemokratie.eu (30.07.2019)", 
                 "Interview BR (11.04.2017)", "Interview ORF (20.05.2017)", "Beitrag auf ddolomiti.eu (08.08.2019)", "Facebook Promovideo (14.03.2017)")

top_all <- dfpubresults[order(dfpubresults$pubresults, decreasing = TRUE),]

# koordinaten der top 10 tage mit plot auf map
head(crowd)
tmp <- str_split(crowd$Erfasst_Am, pattern = " ")
tmp <- sapply(tmp, `[[`, 1)
crowd$day <- tmp
crowd$day <- as.Date(crowd$day)
top10$dates <- as.Date(top10$dates)
final <- crowd[crowd$day %in% top10$dates,]

top10map <- ggmap(map) +
  geom_count(aes(x = lng, y = lat), color = "red", alpha = 0.5, data = final)

top10map

# crowd muss lon & lat spalten haben
# funktion um die top x aktion zu plotten. range gibt an wie viele tage nach den 
# publicity aktionen beruecksichtigt werden sollen
get_topxmap <- function(top = 1, dta = top10, mp = map, range = 1) {
  topx <- dta[top,] 
  tmp <- str_split(crowd$Erfasst_Am, pattern = " ")
  tmp <- sapply(tmp, `[[`, 1)
  crowd$day <- tmp
  crowd$day <- as.Date(crowd$day)
  topx$dates <- as.Date(topx$dates)
  
  li <- list()
  for (i in seq_len(range)) {
    # range 1 => nur am tag der aktion; range 2 => tag der aktion + folgetag etc
    li[[i]] <- crowd[crowd$day %in% (topx$dates + i - 1),]
  }
  final <- dplyr::bind_rows(li)
  
  topxmap <- ggmap(mp) +
    geom_count(aes(x = lng, y = lat), color = "red", alpha = 0.5, data = final) +
    ggtitle(topx$title) +
    #scale_size_continuous(limits=c(1,250))
    scale_size_identity(trans = "sqrt", guide = "legend")
  topxmap$labels$size <- "Anzahl der Einträge"
  topxmap
}

top1map <- get_topxmap(top = 1, range = 2)
# ggsave("top1map.png", plot = top1map, width = 16, height = 10, units = "cm")

for (i in 1:10) {
  tmp <- get_topxmap(top = i, range = 2)
  ggsave(paste0("top", i, "map.png"), plot = tmp, width = 16, height = 10, units = "cm")
}


#plot für alle Aktionen über 2 Tage für alle 43 Aktionen
top_all <- dfpubresults[order(dfpubresults$pubresults, decreasing = TRUE),][1:43,]
top_all$title <- c("Dialekt Themenwoche im BR (27.04.2018)", "Artikel auf rosenheim 24.de (19.06.2018)", "Beitrag auf Facebookseite Servizio Minoranze (09.08.2019)", 
                   "Artikel auf zalp.ch (27.03.2017)", "Artikel auf lmu.de (15.05.2017)", "Bericht auf brennerbasisdemokratie.eu (30.07.2019)", 
                   "Interview BR (11.04.2017)", "Interview ORF (20.05.2017)", "Beitrag auf ddolomiti.eu (08.08.2019)", "Facebook Promovideo (14.03.2017)", " Beitrag in der Facebook-Gruppe 'Solo alpeggio' (05.12.2017)", "Beitrag auf Facebook 'beste Gemeinde' (16.05.2017)", "Facebook: 3000 Belege (30.03.2017)", "Artikeln in 'der Bote' (23.05.2017)", "Facebook: 4000 Belege (13.05.2017)", "offizieller Start Crowdsourcing (Facebook-Post) (10.02.2017)", "Artikel im 'il Gazzettino di Belluno' (04.08.2019)", "www.milchhandwerk.info (04.04.2017)", "Facebook: neuer Flyer (20.04.2017)", "Flyer-Verteilung in Colle S. Lucia (24.08.2019)", "VerbaAlpina Champion 2/17 (28.02.2017)", "Vortrag Bayerischer Almbauerntag (07.10.2017)", "Post auf Reddit.com/austria (20.06.2018)", "Artikel auf bergwelten.com (17.03.2017)", 
                   "VerbaAlpina Champion 3/17 (01.04.2017)", "Bayern2 'Tagesgespräch' (21.02.2019)", "VerbaAlpina Champion 4/17 (01.05.2017)", "Post auf Reddit.com/france und Reddit.com/schweiz (02.07.2018)", "Vorschau auf lausc.it (23.07.2019)", "Artikel in der La Usc di Ladins (26.07.2019)", "Beitrag in der Facebook-Gruppe 'Allevatori italiani' (04.12.2017)", " Vortrag beim Almlehrkurs in Bad Feilnbach (16.02.2018)", " Interview Radio Regenbogen (10.06.2018)", " Facebook: Weihnachtsgruß (19.12.2016)", "Facebook: französischer Spitzenreiter (29.06.2017)", "Vortrag in Sils-Maria (27.12.2017)", "Artikel in 'Anzeiger von Saanen' (23.02.2018)", 
                   "Artikel Zeitschrift Tegernseer Tal (25.09.2018)", "Artikel auf tourentipp.de (23.10.2018)", " Artikel im Corriere delle Alpi (24.07.2019)", " Flyer-Verteilung im Rosengarten/Catinaccio (02.08.2019)", "Artikel im L'Adige - Quotidiano indipendente del Trentino Alto Adige (23.08.2019)", " Flyer-Verteilung bei Poetry Slam St. Ulrich in Gröden (31.08.2019)")
# auskommentieren um es auszufuehren
# for (i in 1:43) {
#   tmp <- get_topxmap(top = i, range = 2)
#   ggsave(paste0("top", i, "map.png"), plot = tmp, width = 16, height = 10, units = "cm")
# }