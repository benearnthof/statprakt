# descriptive analysis of the crowdsourcing data
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
