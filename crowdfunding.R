# descriptive analysis of the crowdsourcing data
crowd <- 
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

typisiert <- read.csv("~/Desktop/wd/statprakt/typisiert.csv", encoding = "UTF8")
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


diffs <- numeric()
for (i in seq_along(dates)) {
  diffs[i] <- dates[i + 1] - dates[i]
}

x <- df$freq
x2 <- data.table::shift(x, n = -1L)
x <- x[-length(x)]
x2 <- x2[-length(x2)]
cor(x, x2)
# better way to calc autocorrelation: stats::acf
stats::acf(df$freq, lag.max = 20, type = "correlation")
