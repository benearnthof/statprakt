# Haeufigste Kategorien 
# 20 verschiede Kategorien (davon aber 1x NA, 1x sonstiges)
crowd <- read.csv("crowd01.csv", encoding = "UTF-8", sep = ",")
table(crowd$Kategorie)
# Haeufigkeiten einzelner Hauptkategorien
# Amphibien        Baeume      Blumen      Fische     Gebaeude      Gefaeße     Gelaende 
# 29                 277          50           9        1786         928         298 
# Geraete         Insekten         N/A    Personal    Produkte   Reptilien  Saeugetiere 
# 1540                175          32          276        2521          31         214 
# Sonstiges      Straeuche Taetigkeiten        Vieh       Voegel     Zutaten 
# 1701                 62        2064        1157         221         726

# Haeufigste Kategorie  = Produkte mit 2521



library(reshape2)
# Plot ueber absolute Haeufigkeiten
test2 <- melt(crowd$Kategorie)
plot_Kategorie <- melt(table(test2))
abs.Kategorie <- ggplot(plot_Kategorie,aes(reorder(x = test2, -value), y = value)) + 
  geom_bar(stat = "identity", fill = "brown1", color = "black", alpha = 0.85) + 
  geom_text(aes(label = value), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "") + 
  ggtitle("Anzahl der Eintraege je Kategorie") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("abs.Kategorie.png", plot = abs.Kategorie, width = 16, height = 13, units = "cm")

mlt <- melt(table(test2))
# relative Haeufigkeiten berechnet
mlt$relative <- mlt$value/(sum(mlt$value))
mlt$relative <- round(mlt$relative, digits = 3)
head(mlt$relative)

#Plot mit relativen Haefigkeiten erstellen
test2 <- melt(crowd$Kategorie)
plot_relative <- melt(table(test2))
plot_relative$relative <- round(plot_relative$value/(sum(plot_relative$value)), digits = 3)

rel.Kategorie <- ggplot(plot_relative,aes(reorder(x = test2, -plot_relative$relative),
                         y = plot_relative$relative)) + 
  geom_bar(stat = "identity", fill = "brown1", color = "black", alpha = 0.85) + 
  geom_text(aes(label = plot_relative$relative), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "rel. Haeufigkeit") + 
  ggtitle("Anteil der Eintraege je Kategorie") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("rel.Kategorie.png", plot = rel.Kategorie, width = 23, height = 12, units = "cm")



                                 
                  



