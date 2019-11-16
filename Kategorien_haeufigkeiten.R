# Häufigste Kategorien 
# 20 verschiede Kategorien (davon aber 1x NA, 1x sonstiges)
table(crowd01$Kategorie)
# Häufigkeiten einzelner Hauptkategorien


#Häufigste Kategorie  = Produkte

plot(sort(table(crowd01$Kategorie)))
