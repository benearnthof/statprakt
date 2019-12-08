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

# tibble <- tibble[3:ncol(tibble)]
tibble <- unique(tibble)

midpoints <- function(x, dp=2){
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(round(lower + (upper - lower) / 2, dp))
}

tibble$x <- midpoints(tibble$xbin)
tibble$y <- midpoints(tibble$ybin)

ggplot(tibble, aes(x, y, z = bin_count)) + stat_summary_2d(bins = 30)

width <- function(x) {
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(upper - lower)
}
mean(width(tibble$xbin)) # 0.4
mean(width(tibble$ybin)) # 0.2

ggplot(tibble, aes(x, y, fill = bin_count, group = bin_count)) +
  geom_bin2d(aes(group = bin_count), binwidth = c(0.4, 0.2), drop = TRUE)

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




