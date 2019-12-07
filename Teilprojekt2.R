## Romanische Sprachfamilie 
# Basistyp Lateinisch
#romanisch <- readRDS("listone.RDS")
#df_rom <- as.data.frame.matrix(romanisch[[1]]@coords)
#df_rom <- distinct(df_rom)

df_rom_lat <- as.data.frame(table(rom_lat_alp$Geo_Data))
df_rom_lat <- subset(df_rom_lat, df_rom_lat$Freq!= 0)
coords_romlat <- get_coords(df_rom_lat$Var1)
df_rom_lat$lng <- coords_romlat$lng
df_rom_lat$lat <- coords_romlat$lat
sp_rom_lat <- df_rom_lat

map_rom <- get_stamenmap(bbox = c(left = 4, bottom = 42, 
                                         right = 18, top = 52), zoom = 6, maptype = "toner-lite") 

plot_rom_lat <- ggmap(map_rom) + 
  stat_bin2d(mapping = aes(x = lng, y = lat), data = sp_rom_lat , bins = 50, alpha = 0.7) +
  scale_fill_gradient("Anzahl", low = "lightsalmon", high = "red") +
  ggtitle("Romanisch mit Basistyp 'Lateinisch'")
plot_rom_lat

ggsave("Romanisch_Lateinisch.png", plot = plot_rom_lat, width = 16, height = 12, units = "cm")

# Basistyp Vorrömisch
#romanisch <- readRDS("listone.RDS")
#df_rom <- as.data.frame.matrix(romanisch[[1]]@coords)
#df_rom <- distinct(df_rom)

df_rom_vor <- as.data.frame(table(rom_vor_alp$Geo_Data))
df_rom_vor <- subset(df_rom_vor, df_rom_vor$Freq != 0)
coords_romvor <- get_coords(df_rom_vor$Var1)
df_rom_vor$lng <- coords_romvor$lng
df_rom_vor$lat <- coords_romvor$lat
sp_rom_vor <- df_rom_vor

map_vor <- get_stamenmap(bbox = c(left = 4, bottom = 42, 
                                  right = 18, top = 52), zoom = 6, maptype = "toner-lite") 

plot_rom_vor <- ggmap(map_vor) + 
  stat_bin2d(mapping = aes(x = lng, y = lat), data = sp_rom_vor , bins = 50, alpha = 0.7) + 
  scale_fill_gradient("Anzahl", low = "skyblue2", high = "slateblue4") +
  ggtitle("Romanisch mit 'Vorrömisch'")
plot_rom_vor 
ggsave("Romanisch_Vorrömisch.png", plot = plot_rom_vor, width = 16, height = 12, units = "cm")

# Germanisch mit Basistypen Vorrömisch und Lateinisch
#germanisch <- readRDS("listtwo.RDS")
#df_germ <- as.data.frame.matrix(germanisch[[1]]@coords)
#df_germ <- distinct(df_germ)

df_germ <- as.data.frame(table(ger_lat_vor_alp$Geo_Data))
df_germ <- subset(df_germ, df_germ$Freq != 0)
coords_germ <- get_coords(df_germ$Var1)


df_germ$lng <- coords_germ$lng
df_germ$lat <- coords_germ$lat
sp_germ <- df_germ


map_germ <- get_stamenmap(bbox = c(left = 4, bottom = 42, 
                                  right = 18, top = 52), zoom = 6, maptype = "toner-lite") 

plot_germ <- ggmap(map_germ) + 
  stat_bin2d(mapping = aes(x = lng, y = lat), data = sp_germ , bins = 50, alpha = 0.8) +
  scale_fill_gradient("Anzahl der Belege", low = "deepskyblue", high = "navy" ) +
  ggtitle("Germanisch mit den Basistypen 'Vorrömisch' und 'Lateinisch'")
plot_germ
ggsave("Germanisch_Basistypen.png", plot = plot_germ, width = 16, height = 12, units = "cm")

# Slawisch
#slawisch <- readRDS("listtre.RDS")
#df_slaw <- as.data.frame.matrix(slawisch[[1]]@coords)
#df_slaw <- distinct(df_slaw)

df_slaw <- as.data.frame(table(slav_lat_vor_alp$Geo_Data))
df_slaw <-subset(df_slaw, df_slaw$Freq != 0)

coords_slaw <- get_coords(df_slaw$Var1)
df_slaw$lng <- coords_slaw$lng
df_slaw$lat <- coords_slaw$lat



sp_slaw <- df_slaw


map_slaw <- get_stamenmap(bbox = c(left = 4, bottom = 42, 
                                   right = 18, top = 52), zoom = 6, maptype = "toner-lite") 
plot_slaw <- ggmap(map_slaw) + 
  stat_bin2d(mapping = aes(x = lng, y = lat), data = sp_slaw , bins = 50, alpha = 0.8) +
  scale_fill_gradient("Anzahl der Belege", low = "orange", high = "orange4") +
  ggtitle("Slavisch mit den Basistypen 'Vorrömisch' und 'Lateinisch'")
plot_slaw

ggsave("Slavisch_Basistypen.png", plot = plot_slaw, width = 16, height = 12, units = "cm")
