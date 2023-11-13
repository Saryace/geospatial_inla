library(SpatialEpi)
data(scotland)
class(scotland)
names(scotland)
head(scotland$data)

map <- scotland$spatial.polygon
plot(map)
# si corro map.. no tiene crs
View(map)
codes <- rgdal::make_EPSG()
codes[which(codes$code == "27700"), ]
proj4string(map) <- "+proj=tmerc +lat_0=49 +lon_0=-2
+k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36
+units=km +no_defs"

#leaflet package to create maps. leaflet expects data to be specified in latitude and longitude using WGS84, so we transform map to this projection as follows:
map <- spTransform(map,
                   CRS("+proj=longlat +datum=WGS84 +no_defs"))


#Preparación de datos
head(scotland$data)
d <- scotland$data[,c("county.names", "cases", "expected", "AFF")]
names(d) <- c("county", "Y", "E", "AFF")

d$SIR <- d$Y / d$E
head(d)

sapply(slot(map, "polygons"), function(x){slot(x, "ID")})

d
library(sp)
rownames(d) <- d$county
d
map <- SpatialPolygonsDataFrame(map, d, match.ID = TRUE)
#ya no $ otra clase de dato
head(map@data)

#Mapping SIRs
#adding the default OpenStreetMap map tiles to the map with addTiles()

library(leaflet)
l <- leaflet(map) %>% addTiles()
pal <- colorNumeric(palette = "YlOrRd", domain = map$SIR)

l %>%
  addPolygons(
    color = "grey", weight = 1,
    fillColor = ~ pal(SIR), fillOpacity = 0.5
  ) %>%
  addLegend(
    pal = pal, values = ~SIR, opacity = 0.5,
    title = "SIR", position = "bottomright"
  )
#adicional
l2 <- leaflet(map) %>% addTiles()
pal2 <- colorNumeric(palette = "YlOrRd", domain = map$Y)

l2 %>%
  addPolygons(
    color = "grey", weight = 1,
    fillColor = ~ pal2(Y), fillOpacity = 0.5
  ) %>%
  addLegend(
    pal = pal2, values = ~Y, opacity = 0.5,
    title = "Y", position = "bottomright"
  )



labels <- sprintf("<strong> %s </strong> <br/>
  Observed: %s <br/> Expected: %s <br/>
  AFF: %s <br/> SIR: %s",
  map$county, map$Y, round(map$E, 2),
  map$AFF, round(map$SIR, 2)
) %>%
  lapply(htmltools::HTML)

l %>%
  addPolygons(
    color = "grey", weight = 1,
    fillColor = ~ pal(SIR), fillOpacity = 0.5,
    highlightOptions = highlightOptions(weight = 4),
    label = labels,
    labelOptions = labelOptions(
      style = list(
        "font-weight" = "normal",
        padding = "3px 8px"
      ),
      textsize = "15px", direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal, values = ~SIR, opacity = 0.5,
    title = "SIR", position = "bottomright"
  )

# Model -------------------------------------------------------------------
#ir ppt
#However, SIRs may be misleading and insufficiently reliable in counties with small populations. In contrast, model-based approaches enable to incorporate covariates and borrow information from neighboring counties to improve local estimates, resulting in the smoothing of extreme values based on small sample sizes. In the next section, we show how to obtain disease risk estimates using a spatial model with the R-INLA package.
library(spdep)
library(INLA)
nb <- poly2nb(map)
head(nb)
nb[[2]] #contains the neighbors of area 2.

nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")

map$idareau <- 1:nrow(map@data)
map$idareav <- 1:nrow(map@data)

formula <- Y ~ AFF +
  f(idareau, model = "besag", graph = g, scale.model = TRUE) +
  f(idareav, model = "iid")

res <- inla(formula,
            family = "poisson", data = map@data,
            E = E, control.predictor = list(compute = TRUE)
)
summary(res)
#This indicates that AFF increases lip cancer risk.
#AFF coefficient by first calculating a smoothing of the marginal distribution of the coefficient with inla.smarginal(), and then using the ggplot() function of the ggplot2 package
library(ggplot2)
marginal <- inla.smarginal(res$marginals.fixed$AFF)
marginal <- data.frame(marginal)
ggplot(marginal, aes(x = x, y = y)) + geom_line() +
  labs(x = expression(beta[1]), y = "Density") +
  geom_vline(xintercept = 0, col = "black") + theme_bw()

head(res$summary.fitted.values)

map$RR <- res$summary.fitted.values[, "mean"]
map$LL <- res$summary.fitted.values[, "0.025quant"]
map$UL <- res$summary.fitted.values[, "0.975quant"]
pal <- colorNumeric(palette = "YlOrRd", domain = map$RR)

labels <- sprintf("<strong> %s </strong> <br/>
  Observed: %s <br/> Expected: %s <br/>
  AFF: %s <br/> SIR: %s <br/> RR: %s (%s, %s)",
  map$county, map$Y, round(map$E, 2),
  map$AFF, round(map$SIR, 2), round(map$RR, 2),
  round(map$LL, 2), round(map$UL, 2)
) %>% lapply(htmltools::HTML)

lRR <- leaflet(map) %>%
  addTiles() %>%
  addPolygons(
    color = "grey", weight = 1, fillColor = ~ pal(RR),
    fillOpacity = 0.5,
    highlightOptions = highlightOptions(weight = 4),
    label = labels,
    labelOptions = labelOptions(
      style =
        list(
          "font-weight" = "normal",
          padding = "3px 8px"
        ),
      textsize = "15px", direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal, values = ~RR, opacity = 0.5, title = "RR",
    position = "bottomright"
  )
lRR

# Probabilidad de excedencia ----------------------------------------------
#ir ppt
#c=2

#marg <- res$marginals.fitted.values[[1]]
#1 - inla.pmarginal(q = 2, marginal = marg)
#aqui se cae
#control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, return.marginals.predictor=TRUE)
res <- inla(formula,
            family = "poisson", data = map@data,
            E = E, control.predictor = list(compute = TRUE),
            control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, return.marginals.predictor=TRUE)
)
#In our example, we can calculate the probability that the relative risk of the first county exceeds 2, 

marg <- res$marginals.fitted.values[[1]]
1 - inla.pmarginal(q = 2, marginal = marg)


exc <- sapply(res$marginals.fitted.values,
              FUN = function(marg){1 - inla.pmarginal(q = 2, marginal = marg)})
map$exc <- exc

pal <- colorNumeric(palette = "YlOrRd", domain = map$exc)

labels <- sprintf("<strong> %s </strong> <br/>
  Observed: %s <br/> Expected: %s <br/>
  AFF: %s <br/> SIR: %s <br/> RR: %s (%s, %s) <br/> P(RR>2): %s",
  map$county, map$Y, round(map$E, 2),
  map$AFF, round(map$SIR, 2), round(map$RR, 2),
  round(map$LL, 2), round(map$UL, 2), round(map$exc, 2)
) %>% lapply(htmltools::HTML)

lexc <- leaflet(map) %>%
  addTiles() %>%
  addPolygons(
    color = "grey", weight = 1, fillColor = ~ pal(exc),
    fillOpacity = 0.5,
    highlightOptions = highlightOptions(weight = 4),
    label = labels,
    labelOptions = labelOptions(
      style =
        list(
          "font-weight" = "normal",
          padding = "3px 8px"
        ),
      textsize = "15px", direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal, values = ~exc, opacity = 0.5, title = "P(RR>2)",
    position = "bottomright"
  )
lexc