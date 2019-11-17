table(nicht_rom$morph_Typ_Sprache)
#gem  sla 
#8971 1305 

table(nicht_rom$Sprache_Basistyp)
#lat  vor 
#7564 2712 

#Bilden von zwei Teilmengen (vorrömisch und lateinisch)
vorrömisch <- subset(nicht_rom, Sprache_Basistyp == "vor")
lateinisch <- subset(nicht_rom, Sprache_Basistyp == "lat")

#Bilden von weiteren vier Teilmengen
gem_vor <- subset(vorrömisch, morph_Typ_Sprache == "gem")
sla_vor <- subset(vorrömisch, morph_Typ_Sprache == "sla")
gem_lat <- subset(lateinisch, morph_Typ_Sprache == "gem")
sla_lat <- subset(lateinisch, morph_Typ_Sprache == "sla")


