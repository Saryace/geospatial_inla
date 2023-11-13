#Desde cap5
library(dplyr)
pennLC$data
dim(pennLC$data)
names(pennLC$data)
summary(pennLC$data)
unique(pennLC$data$county)

d <- pennLC$data %>%  group_by(county) %>% summarize(Y = sum(cases))
head(d)
#We can do this by using the expected() function of SpatialEpi. 
#This function has three arguments, namely,
#population: vector of population counts for each strata in each area,
#cases: vector with the number of cases for each strata in each area,
#n.strata: number of strata.
#There are 2 races, 2 genders and 4 age groups for each county,
#so number of strata is set to 2 x 2 x 4 = 16.
#Vectors population and cases need to be sorted

pennLC$data <- pennLC$data[order(
  pennLC$data$county,
  pennLC$data$race,
  pennLC$data$gender,
  pennLC$data$age
), ]

E <- expected(
  population = pennLC$data$population,
  cases = pennLC$data$cases, n.strata = 16
)
View(data.frame(E))

d$E <- E[match(d$county, unique(pennLC$data$county))]
head(d)
tail(d)
d$SIR <- d$Y / d$E
View(d)
map <- merge(map, d)
mapsf <- st_as_sf(map)
ggplot(mapsf) + geom_sf(aes(fill = SIR)) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red"
  ) +
  geom_text(aes(long, lat, label =county), color = "black") +
  theme_bw()