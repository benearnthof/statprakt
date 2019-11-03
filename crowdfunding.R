# descriptive analysis of the crowdsourcing data
library(ggplot2)
plot(crowd$Kategorie)
ggplot(data = crowd, mapping = aes(x = Kategorie)) + 
  geom_bar()
