#####################################
#### Download Daten Verba Alpine ####
#####################################

## Ordner erstellen, wo die conceopt Liste gespeichert wird
## diesen Ordner als seed setzen, noch einen Unterordner namens data erstellen
#setwd("Neuer Ordner")

## Konzepte einlesen und nur auf die benötigten Konzepte beschränken
concepts <- read.csv("list_182.csv", header = FALSE)
concepts <- concepts[grep("C", concepts$V1),]

## Daten downloaden und speichern (dauert bisschen)
for (a in concepts$V1) {
  try(download.file(paste("https://www.verba-alpina.gwi.uni-muenchen.de/?api=1&action=getRecord&id=", 
                      a, "&version=191&format=csv&empty=0", sep = ""), 
                destfile = paste("data/data_", a, ".csv", sep = "")))
}

## Test: wurden alle Dateien heruntergeladen?
sum(paste("data_", concepts$V1, ".csv", sep = "") %in% list.files("./data")) == nrow(concepts)

## Daten einlesen (als Liste, einzelne Teildatensätze als Listeneinträge):
data <- lapply(concepts$V1, function(a) {
  read.csv(paste("data/data_", a, ".csv", sep = ""))
 }
)
