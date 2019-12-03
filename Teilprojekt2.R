# Romanischer Sprachraum, lat. Inschriften
romanisch <- readRDS("listone.RDS")
df_rom <- as.data.frame.matrix(romanisch[[1]]@coords)
df_rom <- distinct(df_rom)

df_rom_lat <- as.data.frame(table(rom_lat_alp$Geo_Data))
coords_romlat <- get_coords(df_rom_lat$Var1)
df_rom_lat$lng <- coords_romlat$lng
df_rom_lat$lat <- coords_romlat$lat
sp_rom_lat <- df_rom_lat

map_rom <- get_stamenmap(bbox = c(left = 4, bottom = 43, 
                                         right = 17.1, top = 48), zoom = 6, maptype = "toner-lite") 

as.data.frame.matrix(sp_rom_lat@coords)
plot_rom <- ggmap(map_rom) + 
  stat_bin2d(mapping = aes(x = lng, y = lat), data = , bins = 30)
