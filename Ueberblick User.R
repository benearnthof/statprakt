#Einen Überblick über die User vom Crowdsourcing-Tool verschaffen
#crowd Id_Informant, wie viele verschiedene User_Ids gibt es?
library(dplyr)
length(unique(crowd$Id_Informant))
# 1043 verschiede User
table(crowd$Id_Informant)
# Häufigkeiten einzelner User_Ids

#Häufigste User_Id = 14858 
v <- crowd$Id_Informant
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- getmode(v)
print(result)

#User_Id 14858 hat 611 Einträge:
crowd %>% 
    select(Id_Informant) %>% 
    filter(Id_Informant == 14858) %>% 
count(Id_Informant==14858)








