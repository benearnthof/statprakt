library(ggplot2)

Hauptkategorie2 <- unique(crowd01$Hauptkategorie)
#[1] Milchverarbeitung      Viehhaltung            Holzverarbeitung      
#[4] Allgemein              Backen                 Ackerbau              
#[7] Fauna                  Flora                  Küche                 
#[10] Zeit                   Landschaftsformationen Wetter                
#12 Levels: Ackerbau Allgemein Backen Fauna Flora Holzverarbeitung ... Zeit

#color_code <- c("Milchverabeitung" = "red", "Viehhaltung" = "blue", "Holzverarbeitung" = "green", "Allgemein" = "yellow", "Backen" = "orange",  )
#ggplot(crowd01, aes(x = Hauptkategorie)) + geom_boxplot()  +
 # theme(axis.text.x = element_text(angle = 90))

A <- table(crowd01$Hauptkategorie)
#Ackerbau              Allgemein                 Backen 
#389                   2047                    412 
#Fauna                  Flora       Holzverarbeitung 
#711                    728                    296 
#Küche Landschaftsformationen      Milchverarbeitung 
#274                    154                   4877 
#Viehhaltung                 Wetter                   Zeit 
#3724                    356                    129 

plot(sort(A, decreasing = TRUE), ylab = "absolute Häufigkeit", 
     las = 2)

 <-sort(A, decreasing = TRUE)

ggplot(crowd01, aes(x = reorder(Hauptkategorie, Hauptkategorie,
                                function(x)-length(x)))) + geom_bar() +theme(axis.text.x = element_text(angle = 90)) + labs(x = "")
 

