# Häufigste Kategorien 
# 20 verschiede Kategorien (davon aber 1x NA, 1x sonstiges)
table(crowd01$Kategorie)
# Häufigkeiten einzelner Hauptkategorien


#Häufigste Kategorie  = Produkte mit 2521



library(reshape2)
test2 <- melt(crowd01$Kategorie)
plot_Kategorie <- melt(table(test2))
ggplot(plot_Kategorie,aes(reorder(x = test2, -value), y = value)) + 
  geom_bar(stat = "identity", fill = "brown1", color = "black", alpha = 0.85) + 
  geom_text(aes(label = value), vjust = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "") + 
  ggtitle ("Anzahl der Einträge je Kategorie") +
  theme(plot.title = element_text(hjust = 0.5))




                                 
                  



