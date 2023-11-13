#continuación
# Spatial modeling of lung cancer in Pennsylvania
library(spdep)
library(INLA)

map <- pennLC$spatial.polygon
plot(map)
d <- data.frame(county = names(map), neigh = rep(0, length(map)))
rownames(d) <- names(map)
map <- SpatialPolygonsDataFrame(map, d, match.ID = TRUE)

map$idarea <- 1:nrow(map@data) #numero de filas
prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01)),
  phi = list(
    prior = "pc",
    param = c(0.5, 2 / 3))
)
plot(map)
nb <- poly2nb(map)
head(nb)
?nb2INLA
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")
Y<- pennLC$data %>%  group_by(county) %>% summarize(Y = sum(cases))
head(Y)
Y<-data.frame(Y)
Y<-Y[,2]
formula <- Y ~ f(idarea, model = "bym2", graph = g, hyper = prior)
res <- inla(formula,
            family = "poisson", data = map@data,
            E = E, control.predictor = list(compute = TRUE)
)
summary(res)
head(res$summary.fitted.values) #contains summaries of the relative risks including the mean posterior and the lower and upper limits of 95% credible intervals of the relative risks
risk<-res$summary.fitted.values

map$RR <- res$summary.fitted.values[, "mean"]
map$LL <- res$summary.fitted.values[, "0.025quant"]
map$UL <- res$summary.fitted.values[, "0.975quant"]

summary(map@data[, c("RR", "LL", "UL")])

mapsf <- st_as_sf(map)

gRR <- ggplot(mapsf) + geom_sf(aes(fill = RR)) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red",
    limits = c(0.7, 1.5)
  ) +
  theme_bw()
plot(gRR)

gLL <- ggplot(mapsf) + geom_sf(aes(fill = LL)) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red",
    limits = c(0.7, 1.5)
  ) +
  theme_bw()
gUL <- ggplot(mapsf) + geom_sf(aes(fill = UL)) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red",
    limits = c(0.7, 1.5)
  ) +
  theme_bw()

#install.packages("cowplot")
library(cowplot) # o patchwork
plot_grid(gRR, gLL, gUL, ncol = 1)




res$summary.random$idarea
mapsf$re <- res$summary.random$idarea[1:67, "mean"]

ggplot(mapsf) + geom_sf(aes(fill = re)) +
  scale_fill_gradient2(
    midpoint = 0, low = "blue", mid = "white", high = "red"
  ) +
  theme_bw()
