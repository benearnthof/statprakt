library(ggplot2)
library(dplyr)
# beispiel von stackexchange.com
employee <- employee <- c('John','Dave','Paul','Ringo','George','Tom','Jim','Harry','Jamie','Adrian')
quality <- c('good', 'bad')
x = runif(4000,0,100)
y = runif(4000,0,100)
employ.data <- data.frame(employee, quality, x, y)

df <- employ.data %>%
  # aufteilen in 5 teile in x und 5 teile in y richtung
  mutate(xbin = cut(x, breaks = seq(0, 100, by = 20)),
         ybin = cut(y, breaks = seq(0, 100, by = 20)),
         # gruppierte durchschnitte pro bin 
         overall_ave = mean(quality == "bad")) %>%
  group_by(xbin, ybin) %>%
  mutate(bin_ave = mean(quality == "bad")) %>%
  ungroup() %>%
  mutate(bin_quality = bin_ave - overall_ave)

ggplot(df, aes(x, y, fill = bin_quality, group = bin_quality)) +
  geom_bin2d(aes(group = bin_quality), binwidth = c(20, 20), drop = TRUE) +
  scale_fill_gradient2(low="darkred", high = "darkgreen") 

# wir haben koordinaten punkte für inschriften & basistypen
# => xbin cut
# => ybin cut
# overall_avg = mean(class)
# 
l8r_g8r <- readRDS("inschriften_in_alpenraum.RDS")
insch.data <- l8r_g8r

gates <- readRDS("alpenraum_polygon.RDS")

# testen des vorgehens auf stichprobe von 500 punkten
sample <- spsample(gates, 500, type = "random")
extent(insch.data)
extent(gates)
insch.data <- as.data.frame.matrix(insch.data@coords)
insch.data$class <- "insch"
# insch.data <- insch.data[1:1000,]
sampl.data <- as.data.frame.matrix(sample@coords)
sampl.data$class <- "sampl"
names(insch.data) <- names(sampl.data)
data <- rbind(insch.data, sampl.data)

df <- data %>% 
  mutate(xbin = cut(x, breaks = 100),
         ybin = cut(y, breaks = 100), 
         overall_ave = mean(class == "sampl")) %>% 
  group_by(xbin, ybin) %>% 
  mutate(bin_ave = mean(class == "sampl")) %>% 
  ungroup() %>% 
  mutate(bin_quality = bin_ave - overall_ave)

ggplot(df, aes(x, y, z = bin_quality)) + stat_summary_2d()
# scheint zu funktionieren
plot(sample)

dta <- data[data$class == "insch",]
test <- data %>% 
  mutate(xbin = cut(x, breaks = 30),
         ybin = cut(y, breaks = 30)) %>% 
  filter(class == "insch") %>% 
  mutate(count_insch = n_distinct(class)) %>% 
  group_by(xbin, ybin) %>% 
  mutate(bin_count = n())
  
tibble <- data[data$class == "insch",] %>% 
  mutate(xbin = cut(x, breaks = 30),
         ybin = cut(y, breaks = 30)) %>% 
  filter(class == "insch") %>% 
  mutate(count_insch = n_distinct(class)) %>% 
  group_by(xbin, ybin) %>% 
  mutate(bin_count = n())

tibble <- tibble[3:ncol(tibble)]
tibble <- unique(tibble)

# funktion um mittelpunkte der boxen zu finden
midpoints <- function(x, dp=2){
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(round(lower + (upper - lower) / 2, dp))
}

tibble$x <- midpoints(tibble$xbin)
tibble$y <- midpoints(tibble$ybin)
tibble$rel <- tibble$bin_count / sum(tibble$bin_count)
tibble$rel <- tibble$rel * 100

# funktion um breite der boxen zu finden
width <- function(x) {
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(upper - lower)
}
mean(width(tibble$xbin)) # 0.4
mean(width(tibble$ybin)) # 0.2

ggplot(tibble, aes(x, y, fill = bin_count, group = bin_count)) +
  geom_bin2d(aes(group = bin_count), binwidth = c(0.4, 0.2), drop = TRUE)

# erzeugen eines canvas als hintergrund
sp_crowd <- readRDS(file = "sp_crowd.RDS")
box <- sp_crowd@bbox
box[2,] <- c(44, 49)
box[,1] <- c(4.5, 43.3)
box[,2] <- c(16.5, 48.5)
map <- get_stamenmap(bbox = box, zoom = 6, maptype = "toner-lite") 
canvas <- ggmap(map)
canvas
saveRDS(canvas, file = "canvas.RDS")

canvas +
  geom_bin2d(aes(x = x, y = y, fill = rel, 
                 group = rel), binwidth = c(0.4, 0.2), drop = TRUE, data = tibble) +
  scale_fill_continuous(limits = c(0, 7), name = "Anteile in %", type = "viridis") +
  ggtitle("Relative Anteile der Inschriften") #+
  #geom_polygon(data = test, aes(x = long, y = lat), col = "red", fill = NA)
  
# einlesen der daten
# polygone der entsprechenden sprachgebiete
sla <- readRDS(file = "sla_polygon.RDS")
ger <- readRDS(file = "ger_polygon.RDS")
rom <- readRDS(file = "rom_polygon.RDS")

# daten der inschriften in den einzelnen sprachgebieten
sla_insch <- readRDS(file = "sla_inschriften.RDS")
ger_insch <- readRDS(file = "ger_inschriften.RDS")
rom_insch <- readRDS(file = "rom_inschriften.RDS")

zling <- read.csv("z_ling.csv", encoding="UTF-8")
zling_points <- get_coords(zling$Geo_Data)

zling_points <- data.frame(lat = as.numeric(zling_points$lat), lng = as.numeric(zling_points$lng))
coordinates(zling_points) <- ~ lng + lat
raster::crs(zling_points) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

sla_types <- sp::over(zling_points, sla)
ger_types <- sp::over(zling_points, ger)
rom_types <- sp::over(zling_points, rom)

sla_types <- zling_points[!is.na(sp::over(zling_points, sla)),]
ger_types <- zling_points[!is.na(sp::over(zling_points, ger)),]
rom_types <- zling_points[!is.na(sp::over(zling_points, rom)),]

plot(sla)
plot(sla_types, add = TRUE)
plot(ger)
plot(ger_types, add = TRUE)
plot(rom)
plot(rom_types, add = TRUE)

# alles funktioniert wie geplant
# erzeuge als naechstes canvas fuer alle drei teilgebiete

map_sla <- get_stamenmap(bbox = sla@bbox, zoom = 9, maptype = "toner-lite") 
canvas_sla <- ggmap(map_sla)
map_ger <- get_stamenmap(bbox = ger@bbox, zoom = 7, maptype = "toner-lite") 
canvas_ger <- ggmap(map_ger)
map_rom <- get_stamenmap(bbox = rom@bbox, zoom = 7, maptype = "toner-lite") 
canvas_rom <- ggmap(map_rom)

test <- broom::tidy(sla)
canvas_sla +
  geom_polygon(data = test, aes(x = long, y = lat), col = "red",fill = NA)

# karten der inschriften die in die jeweiligen areale fallen
sla_insch_data <- as.data.frame.matrix(sla_insch@coords)
rom_insch_data <- as.data.frame.matrix(rom_insch@coords)
ger_insch_data <- as.data.frame.matrix(ger_insch@coords)
sla_types_data <- as.data.frame.matrix(sla_types@coords)
rom_types_data <- as.data.frame.matrix(rom_types@coords)
ger_types_data <- as.data.frame.matrix(ger_types@coords)

# funktion um daten in gebrauchtes format zu bringen
get_tibble <- function(data, ybreaks, xbreaks) {
tibble <- data %>% 
  mutate(xbin = cut(lng, breaks = xbreaks),
         ybin = cut(lat, breaks = ybreaks), 
         class = "class") %>% 
  mutate(count_insch = n_distinct(class)) %>% 
  group_by(xbin, ybin) %>% 
  mutate(bin_count = n())

tibble <- tibble[3:ncol(tibble)]
tibble <- unique(tibble)
tibble$x <- midpoints(tibble$xbin)
tibble$y <- midpoints(tibble$ybin)
tibble$rel <- tibble$bin_count / sum(tibble$bin_count)
tibble$rel <- tibble$rel * 100
tibble
}

sla_insch_tibble <- get_tibble(sla_insch_data, 20, 20)
ggplot(sla_insch_tibble, aes(x, y, fill = bin_count, group = bin_count)) +
  geom_bin2d(aes(group = bin_count), 
             binwidth = c(mean(width(sla_insch_tibble$xbin)),
                          mean(width(sla_insch_tibble$ybin))), drop = TRUE)

ger_insch_tibble <- get_tibble(ger_insch_data, 30, 30)
ggplot(ger_insch_tibble, aes(x, y, fill = bin_count, group = bin_count)) +
  geom_bin2d(aes(group = bin_count), 
             binwidth = c(mean(width(ger_insch_tibble$xbin)),
                          mean(width(ger_insch_tibble$ybin))), drop = TRUE)
rom_insch_tibble <- get_tibble(rom_insch_data, 30, 30)
ggplot(rom_insch_tibble, aes(x, y, fill = bin_count, group = bin_count)) +
  geom_bin2d(aes(group = bin_count), 
             binwidth = c(mean(width(rom_insch_tibble$xbin)),
                          mean(width(rom_insch_tibble$ybin))), drop = TRUE)

# karten der basistypen die in die jeweiligen gebiete fallen
sla_types_tibble <- get_tibble(sla_types_data, 20, 20)
ggplot(sla_types_tibble, aes(x, y, fill = bin_count, group = bin_count)) +
  geom_bin2d(aes(group = bin_count), 
             binwidth = c(mean(width(sla_types_tibble$xbin)),
                          mean(width(sla_types_tibble$ybin))), drop = TRUE)

ger_types_tibble <- get_tibble(ger_types_data, 30, 30)
ggplot(ger_types_tibble, aes(x, y, fill = bin_count, group = bin_count)) +
  geom_bin2d(aes(group = bin_count), 
             binwidth = c(mean(width(ger_types_tibble$xbin)),
                          mean(width(ger_types_tibble$ybin))), drop = TRUE)

rom_types_tibble <- get_tibble(rom_types_data, 30, 30 )
ggplot(rom_types_tibble, aes(x, y, fill = bin_count, group = bin_count)) +
  geom_bin2d(aes(group = bin_count), 
             binwidth = c(mean(width(rom_types_tibble$xbin)),
                          mean(width(rom_types_tibble$ybin))), drop = TRUE)

# fulljoin die tibbles von inschriften und basistypen um sie vergleichbar zu machen