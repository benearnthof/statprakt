#crowd01 Id_Informant, wie viele verschiedene User_Ids gibt es?
library(dplyr)
length(unique(crowd01$Id_Informant))
# 1043 verschiede User
table(crowd01$Id_Informant)
# Häufigkeiten einzelner User_Ids

#Häufigster Wert (User_Id mit meisten Einträgen) = 14858
v <- crowd01$Id_Informant
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- getmode(v)
print(result)

#User_Id 14858 hat 611 Einträge:
crowd01 %>% 
    select(Id_Informant) %>% 
    filter(Id_Informant == 14858) %>% 
count(Id_Informant==14858)







