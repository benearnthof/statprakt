# Häufigste Kategorien 
# 20 verschiede Kategorien (davon aber 1x NA, 1x sonstiges)
table(crowd01$Kategorie)
# Häufigkeiten einzelner Hauptkategorien
#Amphibien        Bäume      Blumen      Fische     Gebäude      Gefäße     Gelände 
#29                 277          50           9        1786         928         298 
#Geräte         Insekten         N/A    Personal    Produkte   Reptilien  Säugetiere 
#1540                175          32          276        2521          31         214 
#Sonstiges      Sträuche Tätigkeiten        Vieh       Vögel     Zutaten 
#1701                 62        2064        1157         221         726

#Häufigste Kategorie  = Produkte mit 2521



library(reshape2)
#Plot über absolute Häufigkeiten
test2 <- melt(crowd01$Kategorie)
plot_Kategorie <- melt(table(test2))
ggplot(plot_Kategorie,aes(reorder(x = test2, -value), y = value)) + 
  geom_bar(stat = "identity", fill = "brown1", color = "black", alpha = 0.85) + 
  geom_text(aes(label = value), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "") + 
  ggtitle ("Anzahl der Einträge je Kategorie") +
  theme(plot.title = element_text(hjust = 0.5))

#relativen Häufigkeiten berechnet
mlt$relative <- mlt$value/(sum(mlt$value))
mlt$relative <- round(mlt$relative, digits = 3)
head(mlt$relative)
#Plot mit relativen Häfigkeiten erstellen
test2 <- melt(crowd01$Kategorie)
plot_relative <- melt(table(test2))
ggplot(plot_relative,aes(reorder(x = test2, -value), y = mlt$relative)) + 
  geom_bar(stat = "identity", fill = "brown1", color = "black", alpha = 0.85) + 
  geom_text(aes(label = mlt$relative), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "") + 
  ggtitle ("Anteil der Einträge je Kategorie") +
  theme(plot.title = element_text(hjust = 0.5))





                                 
                  



