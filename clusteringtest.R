# lets try clustering
library(sp)
library(rgdal)
library(geosphere)

# example data from the thread
x <- c(-1.482156, -1.482318, -1.482129, -1.482880, -1.485735, -1.485770, -1.485913, -1.484275, -1.485866)
y <- c(54.90083, 54.90078, 54.90077, 54.90011, 54.89936, 54.89935, 54.89935, 54.89879, 54.89902)

# convert data to a SpatialPointsDataFrame object
xy <- SpatialPointsDataFrame(
  matrix(c(x,y), ncol=2), data.frame(ID=seq(1:length(x))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="complete")

# define the distance threshold, in this case 40 m
d=100000

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
xy$clust <- cutree(hc, h=d)

# visualizing
library(dismo)
library(rgeos)

# expand the extent of plotting frame
xy@bbox[] <- as.matrix(extend(extent(xy),0.001))

# get the centroid coords for each cluster
cent <- matrix(ncol=2, nrow=max(xy$clust))
for (i in 1:max(xy$clust))
  # gCentroid from the rgeos package
  cent[i,] <- gCentroid(subset(xy, clust == i))@coords

# compute circles around the centroid coords using a 40m radius
# from the dismo package
ci <- circles(cent, d=d, lonlat=T)

# plot
plot(ci@polygons, axes=T)
plot(xy, col=rainbow(63)[factor(xy$clust)], add=T)

xy <- latvor_coords
proj4string(xy) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
