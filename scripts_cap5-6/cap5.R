library(sf)
library(ggplot2)
#install.packages('SpatialEpi')
library(SpatialEpi)
library(spdep)


class(pennLC)
View(pennLC)
pennLC
View(pennLC$data)
map <- pennLC$spatial.polygon
plot(map)

class(map)
nb <- spdep::poly2nb(map, queen=TRUE)
head(nb)
View(nb)
nb2 <- spdep::poly2nb(map, queen=FALSE)
head(nb2)
View(nb)
nb[[2]]
nb2[[2]]

d <- data.frame(county = names(map), neigh = rep(0, length(map)))
rownames(d) <- names(map)
map <- SpatialPolygonsDataFrame(map, d, match.ID = TRUE)
map$neigh[nb[[2]]] <- 1 #Ver
map$neigh[nb[[44]]] <- 1
map$neigh[nb[[58]]] <- 1

coord <- coordinates(map)
map$long <- coord[, 1]
map$lat <- coord[, 2]
map$ID <- 1:dim(map@data)[1]
mapsf <- st_as_sf(map)
plot(mapsf) #adicional

ggplot(mapsf) + geom_sf(aes(fill = as.factor(neigh))) +
  geom_text(aes(long, lat, label = ID), color = "white") +
  theme_bw() + guides(fill = "none")#FALSE