library(ggplot2)
library(dplyr)
library(tidyr)

employee <- employee <- c('John','Dave','Paul','Ringo','George','Tom','Jim','Harry','Jamie','Adrian')
quality <- c('good', 'bad')
x = runif(4000,0,100)
y = runif(4000,0,100)
employ.data <- data.frame(employee, quality, x, y)

df <- employ.data %>%
  # aufteilen in 5 schnitte in x und y richtung
  mutate(xbin = cut(x, breaks = seq(0, 100, by = 20)),
         ybin = cut(y, breaks = seq(0, 100, by = 20)),
         overall_ave = mean(quality == "bad")) %>%
  # gruppierte mittelwerte bzw counts in unserem fall
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
plot(sample)
# scheint zu funktionieren wie geplant

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

# funktion um mittelpunkte der rechtecke zu finden
midpoints <- function(x, dp=2){
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(round(lower + (upper - lower) / 2, dp))
}

tibble$x <- midpoints(tibble$xbin)
tibble$y <- midpoints(tibble$ybin)
tibble$rel <- tibble$bin_count / sum(tibble$bin_count)
tibble$rel <- tibble$rel * 100

# ggplot(tibble, aes(x, y, z = bin_count)) + stat_summary_2d(bins = 30)

# funktion um breite der rechtecke zu finden 
width <- function(x) {
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(upper - lower)
}
mean(width(tibble$xbin)) # 0.4
mean(width(tibble$ybin)) # 0.2

ggplot(tibble, aes(x, y, fill = bin_count, group = bin_count)) +
  geom_bin2d(aes(group = bin_count), binwidth = c(0.4, 0.2), drop = TRUE)

# canvas der als hintergrund fuer die plots dient
sp_crowd <- readRDS(file = "sp_crowd.RDS")
box <- sp_crowd@bbox
box[2,] <- c(44, 49)
box[,1] <- c(4.5, 43.3)
box[,2] <- c(16.5, 48.5)
map <- get_stamenmap(bbox = box, zoom = 6, maptype = "toner-lite") 
canvas <- ggmap(map)
canvas

canvas +
  geom_bin2d(aes(x = x, y = y, fill = rel, 
                 group = rel), binwidth = c(0.4, 0.2), drop = TRUE, data = tibble) +
  scale_fill_continuous(limits = c(0, 7), name = "Anteile in %", type = "viridis") +
  ggtitle("Anteile der Inschriften") #+
#geom_polygon(data = test, aes(x = long, y = lat), col = "red", fill = NA)


# einlesen der daten
# polygone die die gebiete eingrenzen
sla <- readRDS(file = "sla_polygon.RDS")
ger <- readRDS(file = "ger_polygon.RDS")
rom <- readRDS(file = "rom_polygon.RDS")

# inschriftendaten
sla_insch <- readRDS(file = "sla_inschriften.RDS")
ger_insch <- readRDS(file = "ger_inschriften.RDS")
rom_insch <- readRDS(file = "rom_inschriften.RDS")

sla_points <- readRDS(file = "sla_points.RDS")
ger_points <- readRDS(file = "ger_points.RDS")
rom_vor_points <- readRDS(file = "rom_vor_points.RDS")
rom_lat_points <- readRDS(file = "rom_lat_points.RDS")

# erzeugen eines canvas fuer alle drei gebiete
library(ggmap)
map_sla <- get_stamenmap(bbox = sla@bbox, zoom = 9, maptype = "toner-lite") 
canvas_sla <- ggmap(map_sla)
map_ger <- get_stamenmap(bbox = ger@bbox, zoom = 7, maptype = "toner-lite") 
canvas_ger <- ggmap(map_ger)
map_rom <- get_stamenmap(bbox = rom@bbox, zoom = 7, maptype = "toner-lite") 
canvas_rom <- ggmap(map_rom)

test <- broom::tidy(sla)
canvas_sla +
  geom_polygon(data = broom::tidy(sla), aes(x = long, y = lat), col = "red",fill = NA)

# karten der inschriften die in die jeweiligen gebiete fallen
sla_insch_data <- as.data.frame.matrix(sla_insch@coords)
rom_vor_insch_data <- as.data.frame.matrix(rom_insch@coords)
rom_lat_insch_data <- as.data.frame.matrix(rom_insch@coords)
ger_insch_data <- as.data.frame.matrix(ger_insch@coords)

sla_types_data <- as.data.frame.matrix(sla_points@coords)
rom_vor_types_data <- as.data.frame.matrix(rom_vor_points@coords)
rom_lat_types_data <- as.data.frame.matrix(rom_lat_points@coords)
ger_types_data <- as.data.frame.matrix(ger_points@coords)


# der folgende auskommentierte block ist nicht mehr aktuell
# get_tibble <- function(data, ybreaks, xbreaks) {
# tibble <- data %>% 
#   mutate(xbin = cut(lng, breaks = xbreaks),
#          ybin = cut(lat, breaks = ybreaks), 
#          class = "class") %>% 
#   mutate(count_insch = n_distinct(class)) %>% 
#   group_by(xbin, ybin) %>% 
#   mutate(bin_count = n())
# 
# tibble <- tibble[3:ncol(tibble)]
# tibble <- unique(tibble)
# tibble$x <- midpoints(tibble$xbin)
# tibble$y <- midpoints(tibble$ybin)
# tibble$rel <- tibble$bin_count / sum(tibble$bin_count)
# tibble$rel <- tibble$rel * 100
# tibble
# }

# sla_insch_tibble <- get_tibble(sla_insch_data, 20, 20)
# ggplot(sla_insch_tibble, aes(x, y, fill = bin_count, group = bin_count)) +
#   geom_bin2d(aes(group = bin_count), 
#              binwidth = c(mean(width(sla_insch_tibble$xbin)),
#                           mean(width(sla_insch_tibble$ybin))), drop = TRUE)
# 
# ger_insch_tibble <- get_tibble(ger_insch_data, 30, 30)
# ggplot(ger_insch_tibble, aes(x, y, fill = bin_count, group = bin_count)) +
#   geom_bin2d(aes(group = bin_count), 
#              binwidth = c(mean(width(ger_insch_tibble$xbin)),
#                           mean(width(ger_insch_tibble$ybin))), drop = TRUE)
# rom_insch_tibble <- get_tibble(rom_insch_data, 30, 30)
# ggplot(rom_insch_tibble, aes(x, y, fill = bin_count, group = bin_count)) +
#   geom_bin2d(aes(group = bin_count), 
#              binwidth = c(mean(width(rom_insch_tibble$xbin)),
#                           mean(width(rom_insch_tibble$ybin))), drop = TRUE)
# 
# # make a map of the basistypen that fall into each area
# sla_types_tibble <- get_tibble(sla_types_data, 20, 20)
# ggplot(sla_types_tibble, aes(x, y, fill = bin_count, group = bin_count)) +
#   geom_bin2d(aes(group = bin_count), 
#              binwidth = c(mean(width(sla_types_tibble$xbin)),
#                           mean(width(sla_types_tibble$ybin))), drop = TRUE)
# 
# ger_types_tibble <- get_tibble(ger_types_data, 30, 30)
# ggplot(ger_types_tibble, aes(x, y, fill = bin_count, group = bin_count)) +
#   geom_bin2d(aes(group = bin_count), 
#              binwidth = c(mean(width(ger_types_tibble$xbin)),
#                           mean(width(ger_types_tibble$ybin))), drop = TRUE)
# 
# rom_types_tibble <- get_tibble(rom_types_data, 30, 30 )
# ggplot(rom_types_tibble, aes(x, y, fill = bin_count, group = bin_count)) +
#   geom_bin2d(aes(group = bin_count), 
#              binwidth = c(mean(width(rom_types_tibble$xbin)),
#                           mean(width(rom_types_tibble$ybin))), drop = TRUE)

# leftjoin the tibbles of inschriften and basistypen for every area
# 
# lj_rom <- left_join(rom_types_tibble, rom_insch_tibble, by = c("x" = "x", "y" = "y"))
# # different levels 
# fj_rom <- full_join(rom_types_tibble, rom_insch_tibble, by = c("x" = "x", "y" = "y"))
# # different levels 
# x1 <- unique(rom_types_tibble$x)
# x2 <- unique(rom_types_tibble$y)

# => make the tibbles from 2 class data, split into equal intervals, save counts of every class in 
# each interval, calculate ratio of the classes in every interval, plot ratio on the maps. 
# also correct for type of basetype => vorrom; lateinisch; etc. 

sla_insch_data$class <- "insch"
rom_vor_insch_data$class <- "insch"
rom_lat_insch_data$class <- "insch"
ger_insch_data$class <- "insch"

sla_types_data$class <- "types"
rom_vor_types_data$class <- "types"
rom_lat_types_data$class <- "types"
ger_types_data$class <- "types"

sla_data <- rbind(sla_insch_data, sla_types_data)
ger_data <- rbind(ger_insch_data, ger_types_data)
rom_data_vor <- rbind(rom_vor_insch_data, rom_vor_types_data)
rom_data_lat <- rbind(rom_lat_insch_data, rom_lat_types_data)

# funktion um tibbles zu erzeugen mit denen plot generiert werden koennen
get_tibble_final <- function(data, ybreaks, xbreaks) {
  tibble <- data %>% 
    mutate(xbin = cut(lng, breaks = xbreaks),
           ybin = cut(lat, breaks = ybreaks)) %>% 
    mutate(counts = 1) 
  
  tibble_insch <- tibble %>% 
    filter(class == "insch") %>% 
    group_by(xbin, ybin) %>% 
    mutate(bin_count_insch = n())
  
  tibble_insch <- tibble_insch[3:ncol(tibble_insch)]
  tibble_insch <- unique(tibble_insch)
  
  tibble_types <- tibble %>% 
    filter(class == "types") %>% 
    group_by(xbin, ybin) %>% 
    mutate(bin_count_types = n())
  
  tibble_types <- tibble_types[3:ncol(tibble_types)]
  tibble_types <- unique(tibble_types)
  
  # fulljoin beider data frames
  fj <- full_join(tibble_insch, tibble_types, by = c("xbin" = "xbin", "ybin" = "ybin"))
  
  fj$bin_count_insch <- tidyr::replace_na(fj$bin_count_insch, 0)
  fj$bin_count_types <- tidyr::replace_na(fj$bin_count_types, 0)
  
  fj$bin_ratio_insch <- (fj$bin_count_insch/sum(fj$bin_count_insch)) * 100
  fj$bin_ratio_types <- (fj$bin_count_types/sum(fj$bin_count_types)) * 100
  fj$bin_ratio_diffs <- fj$bin_ratio_types - fj$bin_ratio_insch
  
  fj$x <- midpoints(fj$xbin)
  fj$y <- midpoints(fj$ybin)

  fj
}

sla_fulljoin <- get_tibble_final(sla_data, 20 , 20)
sla_insch_tibble <- sla_fulljoin %>% filter(class.x == "insch")
ggplot(sla_insch_tibble, aes(x, y, fill = bin_ratio_insch, group = bin_ratio_insch)) +
  geom_bin2d(aes(group = bin_ratio_insch), 
             binwidth = c(mean(width(sla_fulljoin$xbin)),
                          mean(width(sla_fulljoin$ybin))), drop = TRUE) +
  xlim(c(13.2, 15.75)) + 
  ylim(c(45.85, 46.65)) + 
  theme(legend.position = "none")

sla_types_tibble <- sla_fulljoin %>% filter(class.y == "types")
ggplot(sla_types_tibble, aes(x, y, fill = bin_ratio_types, group = bin_ratio_types)) +
  geom_bin2d(aes(group = bin_ratio_types), 
             binwidth = c(mean(width(sla_fulljoin$xbin)),
                          mean(width(sla_fulljoin$ybin))), drop = TRUE) +
  xlim(c(13.2, 15.75)) + 
  ylim(c(45.85, 46.65)) + 
  theme(legend.position = "none")

ggplot(sla_fulljoin, aes(x, y, fill = bin_ratio_diffs, group = bin_ratio_diffs)) + 
  geom_bin2d(aes(group = bin_ratio_diffs),
             binwidth = c(mean(width(sla_fulljoin$xbin)),
                          mean(width(sla_fulljoin$ybin))), drop = TRUE) +
  xlim(c(13.2, 15.75)) + 
  ylim(c(45.85, 46.65)) + 
  theme(legend.position = "none")

# sla % diffs ====
canvas_sla +
  geom_bin2d(aes(x = x, y = y, group = bin_ratio_diffs, fill = bin_ratio_diffs), 
             binwidth =c(mean(width(sla_fulljoin$xbin)),
                         mean(width(sla_fulljoin$ybin))), drop = TRUE, data = sla_fulljoin) +
  theme(legend.title = element_blank()) +
  geom_polygon(data = broom::tidy(sla), aes(x = long, y = lat), col = "red",fill = NA) +
  ggtitle("Prozentpunktdifferenz: Basistypen - Inschriften") +
  scale_fill_continuous(limits = c(-15, 10), type = "viridis")

# sla types ====
canvas_sla +
  geom_bin2d(aes(x = x, y = y, group = bin_ratio_types, fill = bin_ratio_types), 
             binwidth =c(mean(width(sla_fulljoin$xbin)),
                         mean(width(sla_fulljoin$ybin))), drop = TRUE, data = sla_types_tibble) +
  theme(legend.title = element_blank()) +
  geom_polygon(data = broom::tidy(sla), aes(x = long, y = lat), col = "red",fill = NA) +
  ggtitle("Relative Anteile: Basistypen") +
  scale_fill_continuous(limits = c(0, 15), type = "viridis")

# sla insch ====
canvas_sla +
  geom_bin2d(aes(x = x, y = y, group = bin_ratio_insch, fill = bin_ratio_insch), 
             binwidth =c(mean(width(sla_fulljoin$xbin)),
                         mean(width(sla_fulljoin$ybin))), drop = TRUE, data = sla_insch_tibble) +
  theme(legend.title = element_blank()) +
  geom_polygon(data = broom::tidy(sla), aes(x = long, y = lat), col = "red",fill = NA) +
  ggtitle("Relative Anteile: Inschriften") +
  scale_fill_continuous(limits = c(0, 15), type = "viridis")

# funktion um differenzen zu plotten
plot_diffs <- function(fulljoin, canvas, polygon, color = "red", limits = c(-15, 10)) {
  plt <- canvas +
    geom_bin2d(aes(x = x, y = y, group = bin_ratio_diffs, fill = bin_ratio_diffs), 
               binwidth =c(mean(width(fulljoin$xbin)),
                           mean(width(fulljoin$ybin))), drop = TRUE, data = fulljoin) +
    theme(legend.title = element_blank()) +
    geom_polygon(data = broom::tidy(polygon), aes(x = long, y = lat), col = color, fill = NA) +
    ggtitle("Prozentpunktdifferenz: Basistypen - Inschriften") +
    scale_fill_continuous(limits = limits, type = "viridis")
  return(plt)
}
sla_diffmap <- plot_diffs(sla_fulljoin, canvas_sla, sla, color = "red") # seems to work
ggsave("sla_diffmap.png", plot = sla_diffmap, width = 18, height = 12, units = "cm")  

# funktion um anteile der basistypen zu plotten 
plot_propo_types <- function(filtered, canvas, polygon, color = "red", fulljoin, 
                             ttl, limits = c(0, 15)) {
  plt <- canvas +
    geom_bin2d(aes(x = x, y = y, group = bin_ratio_types, fill = bin_ratio_types), 
               binwidth =c(mean(width(fulljoin$xbin)),
                           mean(width(fulljoin$ybin))), drop = TRUE, data = filtered) +
    theme(legend.title = element_blank()) +
    geom_polygon(data = broom::tidy(polygon), aes(x = long, y = lat), col = color, fill = NA) +
    ggtitle(paste0("Prozentanteile: ", ttl)) +
    scale_fill_continuous(limits = limits, type = "viridis")
  return(plt)
}

sla_typesmap <- plot_propo_types(filtered = sla_types_tibble, canvas = canvas_sla, polygon = sla, color = "red",
                                 fulljoin = sla_fulljoin, ttl = "Basistypen")
ggsave("sla_typesmap.png", plot = sla_typesmap, width = 18, height = 12, units = "cm")  

# funktion um anteile der inschriften zu plotten
plot_propo_insch <- function(filtered, canvas, polygon, color = "red", fulljoin, ttl,
                             limits = c(0, 15)) {
  plt <- canvas +
    geom_bin2d(aes(x = x, y = y, group = bin_ratio_insch, fill = bin_ratio_insch), 
               binwidth =c(mean(width(fulljoin$xbin)),
                           mean(width(fulljoin$ybin))), drop = TRUE, data = filtered) +
    theme(legend.title = element_blank()) +
    geom_polygon(data = broom::tidy(polygon), aes(x = long, y = lat), col = color, fill = NA) +
    ggtitle(paste0("Prozentanteile: ", ttl)) +
    scale_fill_continuous(limits = limits, type = "viridis")
  return(plt)
}
sla_inschmap <- plot_propo_insch(filtered = sla_insch_tibble, canvas = canvas_sla, polygon = sla, color = "red",
                                 fulljoin = sla_fulljoin, ttl = "Inschriften")
ggsave("sla_inschmap.png", plot = sla_inschmap, width = 18, height = 12, units = "cm")  

# plots for germanic area ==== 
ger_fulljoin <- get_tibble_final(ger_data, 30 , 30)
ger_insch_tibble <- ger_fulljoin %>% filter(class.x == "insch")
ger_types_tibble <- ger_fulljoin %>% filter(class.y == "types")

one <- readRDS("listone.RDS")
two <- readRDS("listtwo.RDS")
tre <- readRDS("listtre.RDS")
fou <- readRDS("listfor.RDS")
fiv <- readRDS("listfiv.RDS")
six <- readRDS("listsix.RDS")
sev <- readRDS("listsev.RDS")

rom <- list(one[[1]])
ger <- list(two[[1]])
colnames(rom[[1]]@coords) <- c("long", "lat")
colnames(ger[[1]]@coords) <- c("long", "lat")

ger_diffmap <- plot_diffs(ger_fulljoin, canvas_ger, ger[[1]]@coords, color = "red", 
                          limits = c(-20, 5)) 
ger_diffmap
ggsave("ger_diffmap.png", plot = ger_diffmap, width = 18, height = 12, units = "cm")  
ger_typesmap <- plot_propo_types(filtered = ger_types_tibble, canvas = canvas_ger,
                                 polygon = ger[[1]]@coords, color = "red",
                                 fulljoin = ger_fulljoin, ttl = "Basistypen")
ger_typesmap
ggsave("ger_typesmap.png", plot = ger_typesmap, width = 18, height = 12, units = "cm")  
ger_inschmap <- plot_propo_insch(filtered = ger_insch_tibble, canvas = canvas_ger,
                                 polygon = ger[[1]]@coords, color = "red",
                                 fulljoin = ger_fulljoin, ttl = "Inschriften")
ger_inschmap
ggsave("ger_inschmap.png", plot = ger_inschmap, width = 18, height = 12, units = "cm")  

# plots for romanic areas ==== 
rom_vor_fulljoin <- get_tibble_final(rom_data_vor, 40 , 40)
rom_vor_insch_tibble <- rom_vor_fulljoin %>% filter(class.x == "insch")
rom_vor_types_tibble <- rom_vor_fulljoin %>% filter(class.y == "types")

rom_vor_diffmap <- plot_diffs(rom_vor_fulljoin, canvas_rom, rom[[1]]@coords, color = "red", 
                              limits = c(-10, 5)) 
rom_vor_diffmap
ggsave("rom_vor_diffmap.png", plot = rom_vor_diffmap, width = 18, height = 12, units = "cm")  
rom_vor_typesmap <- plot_propo_types(filtered = rom_vor_types_tibble, canvas = canvas_rom,
                                     polygon = rom[[1]]@coords, color = "red",
                                     fulljoin = rom_vor_fulljoin, ttl = "Basistypen",
                                     limits = c(0, 10))
rom_vor_typesmap
ggsave("rom_vor_typesmap.png", plot = rom_vor_typesmap, width = 18, height = 12, units = "cm")  
rom_vor_inschmap <- plot_propo_insch(filtered = rom_vor_insch_tibble, canvas = canvas_rom,
                                     polygon = rom[[1]]@coords, color = "red",
                                     fulljoin = rom_vor_fulljoin, ttl = "Inschriften",
                                     limits = c(0, 10))
rom_vor_inschmap
ggsave("rom_vor_inschmap.png", plot = rom_vor_inschmap, width = 18, height = 12, units = "cm")

# rom lat ==== 
rom_lat_fulljoin <- get_tibble_final(rom_data_lat, 40 , 40)
rom_lat_insch_tibble <- rom_lat_fulljoin %>% filter(class.x == "insch")
rom_lat_types_tibble <- rom_lat_fulljoin %>% filter(class.y == "types")

rom_lat_diffmap <- plot_diffs(rom_lat_fulljoin, canvas_rom, rom[[1]]@coords, color = "red", 
                              limits = c(-10, 5)) 
rom_lat_diffmap
ggsave("rom_lat_diffmap.png", plot = rom_lat_diffmap, width = 18, height = 12, units = "cm")  
rom_lat_typesmap <- plot_propo_types(filtered = rom_lat_types_tibble, canvas = canvas_rom,
                                     polygon = rom[[1]]@coords, color = "red",
                                     fulljoin = rom_lat_fulljoin, ttl = "Basistypen",
                                     limits = c(0, 10))
rom_lat_typesmap
ggsave("rom_lat_typesmap.png", plot = rom_lat_typesmap, width = 18, height = 12, units = "cm")  
rom_lat_inschmap <- plot_propo_insch(filtered = rom_lat_insch_tibble, canvas = canvas_rom,
                                     polygon = rom[[1]]@coords, color = "red",
                                     fulljoin = rom_lat_fulljoin, ttl = "Inschriften",
                                     limits = c(0, 10))
rom_lat_inschmap
ggsave("rom_lat_inschmap.png", plot = rom_lat_inschmap, width = 18, height = 12, units = "cm")
