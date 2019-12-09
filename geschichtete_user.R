#1 Eintrag - 313 User
#2 - 10 Einträge = 2131 User gesamt


ids <- table(crowd$Id_Informant)
sum(ids[ids == 1])
sum(ids[ids > 1 & ids < 11])

ids[ids > 1 & ids < 11]
beitrag_2_10 <- ids[ids > 1 & ids < 11]
dimnames(beitrag_2_10) 
#as.numeric(dimnames(beitrag_2_10))
#as.integer(dimnames(beitrag_2_10))
index_2_10 <- as.numeric(dimnames(beitrag_2_10)[[1]])
subset_2_10 <-crowd[crowd$Id_Informant %in% index_2_10, ]
nrow(subset_2_10)
subset_2_10

# 11 - 40 Eintragungen = 4240 User gesamt
ids[ids > 10 & ids < 41 ]
beitrag_11_40 <- ids[ids > 10 & ids < 41]
dimnames(beitrag_11_40)
#as.numeric(dimnames(beitrag_2_40))
#as.integer(dimnames(beitrag_2_40))
index_11_40 <- as.numeric(dimnames(beitrag_11_40)[[1]])
subset_11_40 <-crowd[crowd$Id_Informant %in% index_11_40, ]
nrow(subset_11_40)
subset_11_40

# 41- 100 Eintragungen = 3427 User insgesamt
ids[ids > 40 & ids < 101 ]
beitrag_41_100 <- ids[ids > 40 & ids < 101]
dimnames(beitrag_41_100)
#as.numeric(dimnames(beitrag_2_40))
#as.integer(dimnames(beitrag_2_40))
index_41_100 <- as.numeric(dimnames(beitrag_41_100)[[1]])
subset_41_100 <-crowd[crowd$Id_Informant %in% index_41_100, ]
nrow(subset_41_100)
subset_41_100
df
