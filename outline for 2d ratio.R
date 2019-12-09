library(ggplot2)
library(dplyr)

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

zling <- read.csv("~/Desktop/wd/statprakt/z_ling.csv", encoding="UTF-8")
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
  geom_polygon(data = test, aes(x = long, y = lat), col = "red",fill = NA)

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

lj_rom <- left_join(rom_types_tibble, rom_insch_tibble, by = c("x" = "x", "y" = "y"))
# different levels 
fj_rom <- full_join(rom_types_tibble, rom_insch_tibble, by = c("x" = "x", "y" = "y"))
# different levels 
x1 <- unique(rom_types_tibble$x)
x2 <- unique(rom_types_tibble$y)

# https://discuss.analyticsvidhya.com/t/how-to-merge-datasets-on-nearest-values-in-r/570
# set the NAs to 0
# calculate percentage point differences for every cell
# make a map of the percentage point differences for all 3 areas





test2 <- data %>% 
  mutate(xbin = cut(x, breaks = 30),
         ybin = cut(y, breaks = 30)) %>% 
  group_by(xbin, ybin) %>% 
  filter(class == "sampl") %>% 
  mutate(count_sampl = n())

lj <- left_join(test, test2, by = c("xbin" = "xbin", "ybin" = "ybin"))

table(lj$count_insch)

test$avg <- test$count_insch/nrow(test)
ggplot(test, aes(x, y)) + geom_bin2d()

ggplot(test2, aes(x, y, z = count_sampl)) + stat_summary_2d()