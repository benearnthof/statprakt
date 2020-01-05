# Alle User teilen wir nun in 7 Gruppen ein, nach ihrer Anzahl der Crowdsourcing- Einträge
crowd <- read.csv("crowd01.csv", encoding = "UTF-8", sep = ",")
ids <- table(crowd$Id_Informant)

#1. Gruppe entspricht alle User die nur einen Eintrag geleistet haben
# = 313 verschiedene User/ 313 Einträge
eintrag_eins <- ids[ids == 1]
dimnames(eintrag_eins)
eintrag_eins <- as.numeric(dimnames(eintrag_eins)[[1]])
eintrag_eins <- crowd[crowd$Id_Informant %in% eintrag_eins, ]
table(eintrag_eins$Hauptkategorie)
# Häufigste Kategorie: Milchverarbeitung (235 Einträge), dann Viehhaltung (49 Einträge), Allgemein (13)

#2. Gruppe: 2 - 10 Einträge = 441 User gesamt mit Gesamt 2131 Beiträgen
ids[ids > 1 & ids < 11]
beitrag_2_10 <- ids[ids > 1 & ids < 11]
dimnames(beitrag_2_10) 
index_2_10 <- as.numeric(dimnames(beitrag_2_10)[[1]])
subset_2_10 <-crowd[crowd$Id_Informant %in% index_2_10, ]
nrow(subset_2_10)
# Anzahl Belege gesamt: 2131
filter_subset_2_10 <- subset_2_10[!duplicated(subset_2_10$Id_Informant),]
# Für user anzahl: 441
nrow(filter_subset_2_10)
# Häufigste genannte Kategorie 
table(subset_2_10$Hauptkategorie)
#         Ackerbau              Allgemein                 Backen                  Fauna 
#            36                    243                     51                     50 
#            Flora       Holzverarbeitung                  Küche Landschaftsformationen 
#            61                     40                     27                     18 
# Milchverarbeitung            Viehhaltung                 Wetter                   Zeit 
#           1119                    460                     18                      8 

# Meiste: Kategorie Milchverarbeitung(1119 Einträge), dann Viehhaltung (460 Einträge), Allgemein(243)




# 3. Gruppe: 11 - 40 Eintragungen = 213 User mit 4240 Beiträgen insgesamt
ids[ids > 10 & ids < 41 ]
beitrag_11_40 <- ids[ids > 10 & ids < 41]
dimnames(beitrag_11_40)
index_11_40 <- as.numeric(dimnames(beitrag_11_40)[[1]])
subset_11_40 <-crowd[crowd$Id_Informant %in% index_11_40, ]
#User 213
filter_11_40 <- subset_11_40[!duplicated(subset_11_40$Id_Informant),]
nrow(filter_11_40)
#Einträge gesamt 4240
nrow(subset_11_40)
table(subset_11_40$Hauptkategorie)
#             Ackerbau              Allgemein                 Backen                  Fauna 
#                 119                    558                    138                    136 
#               Flora       Holzverarbeitung                  Küche Landschaftsformationen 
#                 137                     95                     41                     29 
#   Milchverarbeitung            Viehhaltung                 Wetter                   Zeit 
#               1820                   1083                     54                     30 
 
# Meiste Kategorie: Milchverarbeitung (1820), Viehhaltung (1083), Allgemein (558)

# 4. Gruppe 41- 100 Eintragungen 
ids[ids > 40 & ids < 101 ]
beitrag_41_100 <- ids[ids > 40 & ids < 101]
dimnames(beitrag_41_100)
index_41_100 <- as.numeric(dimnames(beitrag_41_100)[[1]])
subset_41_100 <-crowd[crowd$Id_Informant %in% index_41_100, ]
nrow(subset_41_100)
# Anzahl Belege gesamt:3427
filter_subset_41_100 <- subset_41_100[!duplicated(subset_41_100$Id_Informant),]
# User anzahl: 53
nrow(filter_subset_41_100)

# Häufigste genannte Kategorie ? 
table(subset_41_100$Hauptkategorie)
#         Ackerbau              Allgemein                 Backen                  Fauna 
#             123                    503                    133                    151 
#           Flora       Holzverarbeitung                  Küche Landschaftsformationen 
#             163                     89                     48                     21 
# Milchverarbeitung            Viehhaltung                 Wetter                   Zeit 
#             1022                   1083                     72                     19 
#Häufigste Kategorie:  Viehhaltung (1083), dann Milchverarbeitung (1022), Allgemein (503)

# 5. Gruppe: 101 - 200 Einträge ins Crowdsourcing-Tool
ids[ids > 100 & ids < 201 ]
beitrag_101_200 <- ids[ids > 100 & ids < 201]
dimnames(beitrag_101_200)
index_101_200 <- as.numeric(dimnames(beitrag_101_200)[[1]])
subset_101_200 <-crowd[crowd$Id_Informant %in% index_101_200, ]
nrow(subset_101_200)
# Anzahl Belege gesamt: 2325
filter_subset_101_200 <- subset_101_200[!duplicated(subset_101_200$Id_Informant),]
# User anzahl: 18
nrow(filter_subset_101_200)

# Häufigste genannte Kategorie ? 
table(subset_101_200$Hauptkategorie)
#           Ackerbau              Allgemein                 Backen                  Fauna 
#               79                    424                     75                    177 
#             Flora       Holzverarbeitung                  Küche Landschaftsformationen 
#               166                     52                     84                     39 
#   Milchverarbeitung            Viehhaltung                 Wetter                   Zeit 
#               427                    650                    109                     43 
# Häufigste Kategorie: Viehhaltung (650), Milchverarbeitung (427), Allgemein (424)

# 6. Gruppe: 201 - 300 Einträge
beitrag_201_300 <- ids[ids > 200 & ids < 301]
dimnames(beitrag_201_300)
index_201_300 <- as.numeric(dimnames(beitrag_201_300)[[1]])
subset_201_300 <-crowd[crowd$Id_Informant %in% index_201_300, ]
nrow(subset_201_300)
# Anzahl Belege gesamt: 634
filter_subset_201_300 <- subset_201_300[!duplicated(subset_201_300$Id_Informant),]
# User anzahl: 3
nrow(filter_subset_201_300)

# Häufigste genannte Kategorie ? 
table(subset_201_300$Hauptkategorie)
# Häufigste Kategorie: Viehaltung (192), dann Milchverarbeitung (146), Allgemein (90)

# 7. Gruppe: PowerUser 2&1
beitrag_301_500 <- ids[ids > 300 & ids < 701]
dimnames(beitrag_301_500)
index_301_500 <- as.numeric(dimnames(beitrag_301_500)[[1]])
subset_301_500 <-crowd[crowd$Id_Informant %in% index_301_500, ]
nrow(subset_301_500)
# Anzahl Belege gesamt: 1027
filter_subset_301_500 <- subset_301_500[!duplicated(subset_301_500$Id_Informant),]
# User Anzahl: 2
nrow(filter_subset_301_500)
# Häufigste genannte Kategorie ? 
table(subset_301_500$Hauptkategorie)
# Häufigste Kategorie: Allgemein (216), dann Viehhaltung (207), Milchverarbeitung (108)

# Gestapeltes Säulendiagramm erstellen: User-Gruppen Verglichen mit deren ausgewählten Kategorien im Crowdsourcing
# Über alle User-Gruppen sind die häufigsten ausgewählten Hauptkategorien: 
# Milchverarbeitung, Viehhaltung, Allgemein
# deswegen Säulendiagramm nur über diese 3 Hauptkategorien betrachtet

#Tabelle über User-Gruppen und deren ausgewählte Hauptkategorien (mit den absoluten Häufigkeiten)
gruppe    <-factor(c(1, 2, 3, 4, 5, 6, 7 ))
milch     <-c(235, 1119, 1820, 1022, 427, 146, 108)
vieh      <-c(49,  460,  1083, 1083, 650, 192, 207 )
allgemein <-c(13,  243,  558,   503, 424,  90, 216 )
User <- data.frame(gruppe,milch, vieh, allgemein)
User
# Tabelle über User-Gruppen mit relativen Häufigkeiten bzgl. der Anzahl an Hauptkategorien
# Als Gruppenname die Anzahl der Einträge definiert
Gruppe    <-factor(c("1", "2-10", "11-40", "41-100", "101-200", "201-300", ">301" ))
Milchverarbeitung     <-c(79.12, 61.42, 52.59, 39.19, 28.45, 34.11, 20.34)
Viehhaltung           <-c(16.5,  25.25, 31.29, 41.52, 43.30, 44.86, 38.98 )
Allgemein             <-c(4.38,  13.33, 16.12, 19.29, 28.25, 21.03, 40.68 )
User_rel <- data.frame(Gruppe,Milchverarbeitung, Viehhaltung, Allgemein)
User_rel
library(reshape2)
User1 <- melt(User_rel, id.var="Gruppe")
User1
# gestapeltes Säulendiagramm erstellen:
library(ggplot2)
User_Gruppen <- ggplot(User1, aes(x = Gruppe, y = value, fill = variable)) + 
  geom_bar(position = "fill", stat = "identity") +
  ggtitle("Eintragungsverhalten der User") +
  theme_bw()+
  labs(x="User gruppiert nach Anzahl der Eintragungen \n (Gruppengröße)", y="Anteil der Eintragungen", fill = "Hauptkategorie")+
  scale_x_discrete(limits=c("1" , "2-10", "11-40", "41-100", "101-200", "201-300", ">301" ),
                   labels=c("1 \n (313)", "2-10 \n (441)", "11-40 \n (213)", "41-100 \n (53)", "101-200 \n (18)", "201-300 \n (3)", ">301 \n (2)"))
User_Gruppen
ggsave("geschichteter_User.png", plot =User_Gruppen , width = 16, height = 10, units = "cm")
