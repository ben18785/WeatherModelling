library(dplyr)
require(rgdal)
require(maptools)
require(ggplot2)
library(tidyverse)

shape <- readOGR(dsn='.', layer='gadm36')

length(shape@polygons)
info <- shape@data

test <- shape@polygons[[1]]@Polygons[[1]]@coords

test_points <- data.frame(x=c(71.1, 70.9), y=c(37.0, 36.6))
plot(test)
points(test_points, col="red")
afghan <- shape[shape$NAME_0=="Afghanistan", ]
dat <- data.frame(Longitude = test_points$x,
                  Latitude =test_points$y,
                  names = c("Safeco Field", "Key Arena"))
# Assignment modified according
coordinates(dat) <- ~ Longitude + Latitude
proj4string(dat) <- proj4string(afghan)
over(dat, afghan)




