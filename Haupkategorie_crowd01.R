library(ggplot2)

Hauptkategorie2 <- unique(crowd01$Hauptkategorie)
#[1] Milchverarbeitung      Viehhaltung            Holzverarbeitung      
#[4] Allgemein              Backen                 Ackerbau              
#[7] Fauna                  Flora                  Küche                 
#[10] Zeit                   Landschaftsformationen Wetter                
#12 Levels: Ackerbau Allgemein Backen Fauna Flora Holzverarbeitung ... Zeit

table(crowd01$Hauptkategorie)
#Ackerbau              Allgemein                 Backen 
#389                   2047                    412 
#Fauna                  Flora       Holzverarbeitung 
#711                    728                    296 
#Küche Landschaftsformationen      Milchverarbeitung 
#274                    154                   4877 
#Viehhaltung                 Wetter                   Zeit 
#3724                    356                    129 


ggplot(crowd01, aes(x = reorder(Hauptkategorie, Hauptkategorie,
  function(x)-length(x)))) + geom_bar(fill = "blue") +
  theme(axis.text.x = element_text(angle = 90)) + labs(x = "") 
 
library("reshape2")

test2 <- melt(crowd01$Hauptkategorie)

mlt <- melt(table(test2))

mlt$test2 <- as.character(mlt$test2)
which(mlt$test2 == "Milchverarbeitung")
mlt$test2[9] <- "Milch"

which(mlt$test2 == "Holzverarbeitung")
mlt$test2[6] <- "Holz"

which(mlt$test2 == "Landschaftsformationen")
mlt$test2[8] <- "Land"

ggplot(mlt, aes(reorder(x = test2, - value), y = value)) + 
  geom_bar(stat = "identity", fill = "brown1") + 
  geom_text(aes(label = value), vjust = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x = "") + 
  ggtitle("Anzahl der Einträge je Hauptkategorie") + theme(plot.title = element_text(hjust = 0.5))

#"Milch" = Milchverarbeitung
#"Holz" = "Holzverarbeitung" 
#"Land" = Landschaftsformationen