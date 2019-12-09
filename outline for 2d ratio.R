library(ggplot2)
library(dplyr)
library(tidyr)

employee <- employee <- c('John','Dave','Paul','Ringo','George','Tom','Jim','Harry','Jamie','Adrian')
quality <- c('good', 'bad')
x = runif(4000,0,100)
y = runif(4000,0,100)
employ.data <- data.frame(employee, quality, x, y)

df <- employ.data %>%
  mutate(xbin = cut(x, breaks = seq(0, 100, by = 20)),
         ybin = cut(y, breaks = seq(0, 100, by = 20)),
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

width <- function(x) {
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(upper - lower)
}
mean(width(tibble$xbin)) # 0.4
mean(width(tibble$ybin)) # 0.2

ggplot(tibble, aes(x, y, fill = bin_count, group = bin_count)) +
  geom_bin2d(aes(group = bin_count), binwidth = c(0.4, 0.2), drop = TRUE)

canvas

canvas +
  geom_bin2d(aes(x = x, y = y, fill = rel, 
                 group = rel), binwidth = c(0.4, 0.2), drop = TRUE, data = tibble) +
  scale_fill_continuous(limits = c(0, 7), name = "Anteile in %", type = "viridis") +
  ggtitle("Relative Anteile der Inschriften") #+
  #geom_polygon(data = test, aes(x = long, y = lat), col = "red", fill = NA)
  

# reading in data
sla <- readRDS(file = "sla_polygon.RDS")
ger <- readRDS(file = "ger_polygon.RDS")
rom <- readRDS(file = "rom_polygon.RDS")

sla_insch <- readRDS(file = "sla_inschriften.RDS")
ger_insch <- readRDS(file = "ger_inschriften.RDS")
rom_insch <- readRDS(file = "rom_inschriften.RDS")

zling <- read.csv("~/statprakt/z_ling.csv", encoding="UTF-8")
# here the partitioning is needed. ==== 
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

# get a canvas for all 3 areas based on their bounding boxes

map_sla <- get_stamenmap(bbox = sla@bbox, zoom = 9, maptype = "toner-lite") 
canvas_sla <- ggmap(map_sla)
map_ger <- get_stamenmap(bbox = ger@bbox, zoom = 7, maptype = "toner-lite") 
canvas_ger <- ggmap(map_ger)
map_rom <- get_stamenmap(bbox = rom@bbox, zoom = 7, maptype = "toner-lite") 
canvas_rom <- ggmap(map_rom)

test <- broom::tidy(sla)
canvas_sla +
  geom_polygon(data = broom::tidy(sla), aes(x = long, y = lat), col = "red",fill = NA)

# make a map of the inschriften that fall into each area
sla_insch_data <- as.data.frame.matrix(sla_insch@coords)
rom_insch_data <- as.data.frame.matrix(rom_insch@coords)
ger_insch_data <- as.data.frame.matrix(ger_insch@coords)
sla_types_data <- as.data.frame.matrix(sla_types@coords)
rom_types_data <- as.data.frame.matrix(rom_types@coords)
ger_types_data <- as.data.frame.matrix(ger_types@coords)

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

# make a map of the basistypen that fall into each area
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
rom_insch_data$class <- "insch"
ger_insch_data$class <- "insch"
sla_types_data$class <- "types"
rom_types_data$class <- "types"
ger_types_data$class <- "types"

sla_data <- rbind(sla_insch_data, sla_types_data)
ger_data <- rbind(ger_insch_data, ger_types_data)
rom_data <- rbind(rom_insch_data, rom_types_data)

get_tibble_final <- function(data, ybreaks, xbreaks) {
tibble <- data %>% 
  mutate(xbin = cut(lng, breaks = 20),
         ybin = cut(lat, breaks = 20)) %>% 
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

fj <- full_join(tibble_insch, tibble_types, by = c("xbin" = "xbin", "ybin" = "ybin"))
# joins correctly 

fj$bin_count_insch <- tidyr::replace_na(fj$bin_count_insch, 0)
fj$bin_count_types <- tidyr::replace_na(fj$bin_count_types, 0)

fj$bin_ratio_insch <- (fj$bin_count_insch/sum(fj$bin_count_insch)) * 100
fj$bin_ratio_types <- (fj$bin_count_types/sum(fj$bin_count_types)) * 100
fj$bin_ratio_diffs <- fj$bin_ratio_types - fj$bin_ratio_insch

# tibble <- tibble[3:ncol(tibble)]
# tibble <- unique(tibble)

fj$x <- midpoints(fj$xbin)
fj$y <- midpoints(fj$ybin)
# tibble$rel <- tibble$bin_count / sum(tibble$bin_count)

fj
}

sla_fulljoin <- get_tibble_final(sla_data)
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

plot_diffs <- function(fulljoin, canvas, polygon, color = "red") {
  plt <- canvas +
    geom_bin2d(aes(x = x, y = y, group = bin_ratio_diffs, fill = bin_ratio_diffs), 
               binwidth =c(mean(width(fulljoin$xbin)),
                           mean(width(fulljoin$ybin))), drop = TRUE, data = fulljoin) +
    theme(legend.title = element_blank()) +
    geom_polygon(data = broom::tidy(polygon), aes(x = long, y = lat), col = color, fill = NA) +
    ggtitle("Prozentpunktdifferenz: Basistypen - Inschriften") +
    scale_fill_continuous(limits = c(-15, 10), type = "viridis")
  return(plt)
}
plot_diffs(sla_fulljoin, canvas_sla, sla, color = "lightblue") # seems to work

plot_propo_types <- function(filtered, canvas, polygon, color = "red", fulljoin, ttl) {
  plt <- canvas +
    geom_bin2d(aes(x = x, y = y, group = bin_ratio_types, fill = bin_ratio_types), 
               binwidth =c(mean(width(fulljoin$xbin)),
                           mean(width(fulljoin$ybin))), drop = TRUE, data = filtered) +
    theme(legend.title = element_blank()) +
    geom_polygon(data = broom::tidy(polygon), aes(x = long, y = lat), col = color, fill = NA) +
    ggtitle(paste0("Relative Anteile: ", ttl)) +
    scale_fill_continuous(limits = c(0, 15), type = "viridis")
  return(plt)
}

plot_propo_types(filtered = sla_types_tibble, canvas = canvas_sla, polygon = sla, color = "green",
           fulljoin = sla_fulljoin, ttl = "Basistypen")

plot_propo_insch <- function(filtered, canvas, polygon, color = "red", fulljoin, ttl) {
  plt <- canvas +
    geom_bin2d(aes(x = x, y = y, group = bin_ratio_insch, fill = bin_ratio_insch), 
               binwidth =c(mean(width(fulljoin$xbin)),
                           mean(width(fulljoin$ybin))), drop = TRUE, data = filtered) +
    theme(legend.title = element_blank()) +
    geom_polygon(data = broom::tidy(polygon), aes(x = long, y = lat), col = color, fill = NA) +
    ggtitle(paste0("Relative Anteile: ", ttl)) +
    scale_fill_continuous(limits = c(0, 15), type = "viridis")
  return(plt)
}
plot_propo_insch(filtered = sla_insch_tibble, canvas = canvas_sla, polygon = sla, color = "green",
                 fulljoin = sla_fulljoin, ttl = "Inschriften")

# sort types by epoch
# plot everything on corresponding maps
# add stacked barcharts for categories in crowdsourcing
# add app to visualize the impact of extending the day range of publicity aktionen
