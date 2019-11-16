#crowd01 Id_Informant, wie viele verschiedene User gibt es?
library(dplyr)
length(unique(crowd01$Id_Informant))
# 1043 verschiede User
table(crowd01$Id_Informant)
# Häufigkeiten einzelner User

#Häufigster Wert (User mit meisten Einträgen) = 14858
v <- crowd01$Id_Informant
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- getmode(v)
print(result)

#User 14858 hat 611 einträge:
crowd01 %>% 
    select(Id_Informant) %>% 
    filter(Id_Informant == 14858) %>% 
count(Id_Informant==14858)





