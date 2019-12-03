# laendergrenzen
ger <- raster::getData("GADM", country = "DEU", level = 1)
plot(ger)
str(ger)
bay <- ger[match(toupper("Bayern"),toupper(ger$NAME_1)),]

plot(bay)
df <- as.data.frame.matrix(bay@polygons[[1]]@Polygons[[1]]@coords)
plot(df$y~df$x)

ggplot(df) +
  geom_polygon(aes(x = x, y = y), fill = NA, col = "black")
