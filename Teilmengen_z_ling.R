##Teilmengen bilden (romanisch, rom_lat, rom_vor)
rom <- subset(z_ling, Type_Lang == "roa")

rom_lat <- subset(rom, Base_Type_Lang == "lat" )
rom_vor <- subset(rom, Base_Type_Lang == "vor")

# 'Alpine_Convention' gibt an, ob sich der Beleg in der Alpenkonvention befindet (1) oder nicht (0)
# d.h. 
rom_lat_alp <- subset(rom_lat, Alpine_Convention == 1) # Beobachtungen im Alpenraum
rom_vor_alp <- subset(rom_vor, Alpine_Convention == 1) # Beobachtungen im Alpenraum

# Belege, wo die Zuordnung der Basistypen unsicher sind
rom_lat_unsure <- subset(rom_lat, Base_Type_Unsure == 1)
# 56 Beobachtungen
rom_vor_unsure <- subset(rom_vor, Base_Type_Unsure == 1)
# 54 Beobachtungen

# Betrachte, welche Basistypen es überhaupt gibt
table(rom$Base_Type_Lang)

#  ahd   deu   eng   fas   fra   frk   gal   gem   goh   got   grc   lat   lts 
#    6    33     2    98     6   115   101  2695    99   204   287 66844    14 

# NULL  roa   sla   vor   xxx 
# 6553    6     3  3562  1247 


##Teilmengen bilden (germanisch, ger_lat, ger_vor)
ger <- subset(z_ling, Type_Lang == "gem")

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
#  1475    73   306     0    20     0   622 30350  9464  1857   655 18282     0 

#  NULL   roa   sla   vor   xxx 
# 41995     0   150  7659   615 


## Teilmengenbilden (slawisch, slaw_lat, slaw_vor)
slaw <- subset(z_ling, Type_Lang == "sla")

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
#  11   58    4    0    0    1    0 1079  471    0    0 2219    0  837    0 3193 

# vor  xxx 
# 140    8 

