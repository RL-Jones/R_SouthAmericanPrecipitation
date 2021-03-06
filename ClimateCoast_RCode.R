#Set Working Directory and Load in packages/files
setwd("~/OneDrive/University_Work/Advanced_Physical/Analysis")
library("vegan")
library("rgeos")
library("maptools")
clim <- read.csv("S_Am_precip.csv", header=T)
#Make another object just containing the monthly proportion of precipitation
cl <- clim[, 21:32]
#Use euclidean distance metric
cl_dist <- vegdist(cl, method="euclid") 
#Create 7 clusters
my_clust <- hclust(cl_dist)
my_clusters <- cutree(my_clust, k=7)
#Plot a map of weather stations, coloured by cluster
#Load in coastline data
llCRS <- CRS("+proj=longlat +datum=WGS84")
#Extracts the coordinates of the weather stations
my_coords <- data.frame(clim$lon, clim$lat)
#Labels the dataframe of coordinates correctly
colnames(my_coords) <- c("Lon", "Lat")
#Uses the coordinates and the list of clusters to make a new spatial points dataframe 
my_sp <- SpatialPointsDataFrame(my_coords, data.frame(my_clusters), proj4string = llCRS, match.ID = FALSE)
land_area <- getRgshhsMap("gshhs_l.b", xlim=as.vector(my_sp@bbox[1,]), ylim=as.vector(my_sp@bbox[2,]))
#specifies how to plot the coastline
land_area <- list("sp.polygons", land_area, col="grey70")
#sets up a list of objects to plot
my_layout <- list()
#adds the coastline to that list
my_layout[[1]] <- land_area
#plots a graph that combines the weather station points, coloured by cluster, along with the coastline data
spplot(my_sp, sp.layout=my_layout,  value=my_clusters, cuts=7, col.regions=c(1:7), auto.key=FALSE)
#Look at the typical climate of each cluster
par(mfrow=c(2,4))
plot(clim$lon, clim$lat, col=my_clusters)
for(i in 1:7){
  x <- apply(cl[my_clusters==i, ], MARGIN=2, FUN=mean)  
  plot(c(1:12), x, col=i, type="n", ylim=c(0,1), ylab="Proportion of annual precip", xlab="Month of year")  
  for(j in 1:12){  
    points(rep(j, length(my_clusters[my_clusters==i])), cl[my_clusters==i, j], col="grey", pch=19)    
  }    
  lines(c(1:12), x, col=i, ylim=c(0,1))  
  points(c(1:12), x, col=i, ylim=c(0,1))}
