z_ling <- read.csv("z_ling.csv", encoding = "UTF-8")
#Duplikate der Sprachbelege entfernen
z_ling2 <- z_ling[!duplicated(z_ling$Id_Instance),]


##Teilmengen bilden (romanisch, rom_lat, rom_vor)
rom <- subset(z_ling2, Type_Lang == "roa")

rom_lat <- subset(rom, Base_Type_Lang == "lat" )
rom_vor <- subset(rom, Base_Type_Lang == "vor")

# Belege, wo die Zuordnung der Basistypen unsicher sind
rom_lat_unsure <- subset(rom_lat, Base_Type_Unsure == 1)
# 56 Beobachtungen
rom_vor_unsure <- subset(rom_vor, Base_Type_Unsure == 1)
# 54 Beobachtungen

#sichere Zuordnung
rom_lat_sure <- subset(rom_lat, Base_Type_Unsure == 0)
rom_vor_sure <- subset(rom_vor, Base_Type_Unsure == 0)

# 'Alpine_Convention' gibt an, ob sich der Beleg in der Alpenkonvention befindet (1) oder nicht (0)
# d.h. 
rom_lat_alp <- subset(rom_lat_sure, Alpine_Convention == 1) # Beobachtungen im Alpenraum
rom_vor_alp <- subset(rom_vor_sure, Alpine_Convention == 1) # Beobachtungen im Alpenraum

# Betrachte, welche Basistypen es überhaupt gibt
table(rom$Base_Type_Lang)

#  ahd   deu   eng   fas   fra   frk   gal   gem   goh   got   grc   lat   lts 
#    6    16     2    37     5   108    73  1503    61    83   170 19733     5 
# NULL   roa   sla   vor   xxx 
# 4430     2     2  1508   481 

##Teilmengen bilden (germanisch, ger_lat, ger_vor)
ger <- subset(z_ling2, Type_Lang == "gem")

ger_lat <- subset(ger, Base_Type_Lang == "lat")
ger_vor <- subset(ger, Base_Type_Lang == "vor")

ger_lat_vor <- subset(ger, c(Base_Type_Lang == "lat", Base_Type_Lang == "vor"))
ger_lat_vor_alp <- subset(ger_lat_vor, Alpine_Convention == 1)

# Belege, wo die Zuordnung der Basistypen unsicher sind
ger_lat_unsure <- subset(ger_lat, Base_Type_Unsure == 1)
ger_vor_unsure <- subset(ger_vor, Base_Type_Unsure == 1)
# jeweils 0 Beobachtungen -> unsure-Beobachtungen existieren nicht


#Welche Basistypen gibt es überhaupt?
table(ger$Base_Type_Lang)

#   ahd   deu   eng   fas   fra   frk   gal   gem   goh   got   grc   lat   lts 
#   296    27     6     0    10     0    92  9173  3361   159    47  5289     0 
#  NULL   roa   sla   vor   xxx 
# 31137     0    12  1880    32 


## Teilmengenbilden (slawisch, slaw_lat, slaw_vor)
slaw <- subset(z_ling2, Type_Lang == "sla")

slaw_lat <- subset(slaw, Base_Type_Lang == "lat")
slaw_vor <- subset(slaw, Base_Type_Lang == "vor")


slaw_vor_lat <- subset(slaw, c(Base_Type_Lang == "lat", Base_Type_Lang == "vor"))
slaw_vor_lat_alp <- subset(slaw_vor_lat, Alpine_Convention == 1)

# Belege, wo die Zuordnung der Basistypen unsicher sind
slaw_lat_unsure <- subset(slaw_lat, Base_Type_Unsure == 1)
slaw_vor_unsure <- subset(slaw_vor, Base_Type_Unsure == 1)
# jeweils 0 Beobachtungen -> unsure-Beobachtungen existieren nicht

#Wie viele Basistypen gibt es überhaupt?
table(slaw$Base_Type_Lang)

# ahd  deu  eng  fas  fra  frk  gal  gem  goh  got  grc  lat  lts NULL  roa  sla 
#  11   58    3    0    0    1    0  825  429    0    0 1157    0  764    0 2793 
# vor  xxx 
# 136    8 

