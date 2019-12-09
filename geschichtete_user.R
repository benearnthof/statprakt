#Alle Beiträge gesamt dfd
all <- ids
dimnames(all)
index_all <- as.numeric(dimnames(all)[[1]])
crowd_all <- crowd[crowd$Id_Informant %in% index_all, ]
nrow(crowd_all)
table(crowd$Hauptkategorie)
#1 Eintrag - 313 User mit je einem Beitrag
beitrag_1 <- ids[ids == 1]
dimnames(beitrag_1)
index_1 <- as.numeric(dimnames(beitrag_1)[[1]])
subset_1 <- crowd[crowd$Id_Informant %in% index_1, ]
nrow(subset_1)
filter_subset_1 <- subset_1[!duplicated(subset_1$Id_Informant),]
nrow(filter_subset_1)
table(subset_1$Hauptkategorie)
#2 - 10 Einträge = 441 User gesamt mit Gesamt 2131 Beiträgen
ids <- table(crowd$Id_Informant)
sum(ids[ids == 1])
sum(ids[ids > 1 & ids < 11])

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


# Häufigste genannte Kategorie ? 
table(subset_2_10$Hauptkategorie)
#         Ackerbau              Allgemein                 Backen                  Fauna 
#            36                    243                     51                     50 
#            Flora       Holzverarbeitung                  Küche Landschaftsformationen 
#            61                     40                     27                     18 
# Milchverarbeitung            Viehhaltung                 Wetter                   Zeit 
#           1119                    460                     18                      8 

# Meiste: Kategorie Milchverarbeitung, dann Viehhaltung




# 11 - 40 Eintragungen = 213 User mit 4240 Beiträgen insgesamt
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
 
# Meiste Kategorie: Milchverarbeitung, Viehhaltung

# 41- 100 Eintragungen 
ids[ids > 40 & ids < 101 ]
beitrag_41_100 <- ids[ids > 40 & ids < 101]
dimnames(beitrag_41_100)
index_41_100 <- as.numeric(dimnames(beitrag_41_100)[[1]])
subset_41_100 <-crowd[crowd$Id_Informant %in% index_41_100, ]
nrow(subset_41_100)
# Anzahl Belege gesamt:3427
filter_subset_41_100 <- subset_41_100[!duplicated(subset_41_100$Id_Informant),]
# Für user anzahl: 53
nrow(filter_subset_41_100)

# Häufigste genannte Kategorie ? 
table(subset_41_100$Hauptkategorie)
#         Ackerbau              Allgemein                 Backen                  Fauna 
#             123                    503                    133                    151 
#           Flora       Holzverarbeitung                  Küche Landschaftsformationen 
#             163                     89                     48                     21 
# Milchverarbeitung            Viehhaltung                 Wetter                   Zeit 
#             1022                   1083                     72                     19 
#Häufigste Kategorie:  Viehhaltung, dann Milchverarbeitung

# 101 - 200
ids[ids > 100 & ids < 201 ]
beitrag_101_200 <- ids[ids > 100 & ids < 201]
dimnames(beitrag_101_200)
index_101_200 <- as.numeric(dimnames(beitrag_101_200)[[1]])
subset_101_200 <-crowd[crowd$Id_Informant %in% index_101_200, ]
nrow(subset_101_200)
# Anzahl Belege gesamt: 2325
filter_subset_101_200 <- subset_101_200[!duplicated(subset_101_200$Id_Informant),]
# Für user anzahl: 18
nrow(filter_subset_101_200)

# Häufigste genannte Kategorie ? 
table(subset_101_200$Hauptkategorie)
#           Ackerbau              Allgemein                 Backen                  Fauna 
#               79                    424                     75                    177 
#             Flora       Holzverarbeitung                  Küche Landschaftsformationen 
#               166                     52                     84                     39 
#   Milchverarbeitung            Viehhaltung                 Wetter                   Zeit 
#               427                    650                    109                     43 
# Häufigste Kategorie: Viehhaltung,Milchverarbeitung 

# 201 - 300 Einträge
beitrag_201_300 <- ids[ids > 200 & ids < 301]
dimnames(beitrag_201_300)
index_201_300 <- as.numeric(dimnames(beitrag_201_300)[[1]])
subset_201_300 <-crowd[crowd$Id_Informant %in% index_201_300, ]
nrow(subset_201_300)
# Anzahl Belege gesamt: 634
filter_subset_201_300 <- subset_201_300[!duplicated(subset_201_300$Id_Informant),]
# Für user anzahl: 3
nrow(filter_subset_201_300)

# Häufigste genannte Kategorie ? 
table(subset_201_300$Hauptkategorie)

# Häufigste Kategorie: Viehaltung, dann Milchverarbeitung

# PowerUser 2&1
beitrag_301_500 <- ids[ids > 300 & ids < 701]
dimnames(beitrag_301_500)
index_301_500 <- as.numeric(dimnames(beitrag_301_500)[[1]])
subset_301_500 <-crowd[crowd$Id_Informant %in% index_301_500, ]
nrow(subset_301_500)
# Anzahl Belege gesamt: 1027
filter_subset_301_500 <- subset_301_500[!duplicated(subset_301_500$Id_Informant),]
# Für user anzahl: 2
nrow(filter_subset_301_500)

# Häufigste genannte Kategorie ? 
table(subset_301_500$Hauptkategorie)

# Häufigste Kategorie: Allgemein, dann Viehhaltung


#Plot erstelllen
#Data frame erstllen
group <- c(1,2,3,4,5,6,7)

milch     <- c(79.12, 61.42, 52.59, 39.19, 28.45, 34.11, 20.34 )
vieh      <- c(16.5, 25.25,31.29,41.52,43.3, 44.86,38.98)
allgemein <- c(4.38, 13.33, 16.12, 19.29, 28.25, 21.03, 40.68 )
crowd_kategorien <- data.frame(milch, vieh, allgemein)

DF <- read.table(text="Rank Milchverarbeitung     Viehhaltung     Allgemein
1         79.12   16.50      4.38
2-10      61.42   25.25     13.33
11-40     52.59   31.29     16.12
41-100    39.19   41.52     19.29
101-200   28.45   43.30     28.25
201-300   34.11   44.86     21.03
301-611   20.34   38.98     40.68", header=TRUE)
library(reshape2)
DF1 <- melt(DF, id.var="Rank")
library(ggplot2)
ggplot(DF1, aes(x = Rank, y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  ylab("Anzahl der Eintragungen") +
  scale_x_discrete(name ="User gruppiert nach Anzahl der Eintragungen", 
                   limits=c("1","2-10","11-40", "41-100", "101-200", "201-300","301-611")) +
  scale_fill_discrete(name = "Hauptkategorie") +
  theme_bw()+
  ggtitle("Eintragungsverhalten der User")
ggsave("gestapelter_Barplot.png", plot = plot_2019, width = 18, height = 12, units = "cm")
  
