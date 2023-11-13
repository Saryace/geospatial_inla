library(spData)
library(spdep)

#?st_read
map3 <- st_read(system.file("shapes/columbus.shp",
                           package = "spData"), quiet = TRUE)
plot(map3)
#
class(map3)
nb3 <- spdep::poly2nb(map3, queen = TRUE) #FALSE TRUE
head(nb3)
plot(st_geometry(map3), border = "lightgray")
plot.nb(nb3, st_geometry(map3), add = TRUE)
#cambiar y correr hasta linea 24
map3
cc<-st_centroid(map3$geometry)
point_x<-st_coordinates(cc)
map3$long3 <- point_x[,1]
map3$lat3 <- point_x[,2]
id <- 5# area id 20 o 5
map3$neighbors <- "other"
map3$neighbors[id] <- "area"
map3$neighbors[nb3[[id]]] <- "neighbors"
names(map3)

ggplot(map3) + geom_sf(aes(fill = neighbors)) + theme_bw() + 
  geom_text(aes(long3,lat3,label = POLYID), color = "black")+
  scale_fill_manual(values = c("gray30", "gray", "white")) #orden alfabetico

coo <- st_centroid(map3)
nb <- knn2nb(knearneigh(coo, k = 3)) # k number nearest neighbors
plot(st_geometry(map3), border = "lightgray")
plot.nb(nb, st_geometry(map3), add = TRUE)

nb3[[5]]
nb[[5]]
#tbn por distancia
#tbn por distancia y por k con segundo orden 
#repetir por si cambie
nb3 <- poly2nb(map3, queen = TRUE)
nbw <- spdep::nb2listw(nb3, style = "W")#W B
nbw$weights[1:3]
nbw$neighbours
nbw$neighbours[20]
m2 <- listw2mat(nbw)
lattice::levelplot(t(m2),
                   scales = list(y = list(at = c(10, 20, 30, 40),
                                          labels = c(10, 20, 30, 40))))