setwd("C:/Users/bclamber/Downloads/gadm36_shp")
library(dplyr)
require(rgdal)
require(maptools)
require(ggplot2)
library(tidyverse)

shape <- readOGR(dsn='.', layer='gadm36')
unique(shape$NAME_1)

burkina <- shape[shape$NAME_0=="Burkina Faso", ]
unique(burkina$NAME_1)
plot(burkina)
length(burkina@polygons)
plot(burkina@polygons[[1]]@Polygons)
map_chr(seq(1, 351, 1), ~burkina@polygons[[.]]@ID)
burkina$id
burkina$NAME_3
a_test <- burkina@data
a_df <- burkina@polygons[[1]]@Polygons[[1]]@coords
plot(a_df[, 1], a_df[, 2])

burkina_admin1_2 <- burkina[burkina$NAME_1=="Boucle du Mouhoun", ]
plot(burkina_admin1_2)

library(sp)
aTest <- spsample(burkina_admin1_2, 1000, type="random")
plot(burkina_admin1_2)
points(aTest, col='red')


# Code to generate 10 points per admin 1 unit (later will probably want to do this in proportion to area)
lNames <- unique(burkina$NAME_1)
lSamples <- vector(mode="list")
a_num_samples <- 100
for(i in seq_along(lNames)){
  a_admin1 <- burkina[burkina$NAME_1==lNames[[i]], ]
  lSamples[[i]] <- spsample(a_admin1, a_num_samples, type="random")
}
lSamples <- do.call(rbind, lSamples)
plot(burkina)
points(lSamples, col='red')

# Generate samples in proportion to the area of admin 1 unit (with a minimum of 1)
lNames <- unique(burkina$NAME_1)
lSamples <- vector(mode="list")
a_num_samples_per_unit_area <- 10
for(i in seq_along(lNames)){
  a_admin1 <- burkina[burkina$NAME_1==lNames[[i]], ]
  tot_area <- sum(map_dbl(a_admin1@polygons, ~.@area))
  a_num_samples <- round(tot_area * a_num_samples_per_unit_area)
  a_num_samples <- if_else(a_num_samples>1, a_num_samples, 1)
  lSamples[[i]] <- spsample(a_admin1, a_num_samples, type="random")
}
lSamples <- do.call(rbind, lSamples)
plot(burkina)
points(lSamples, col='red')

# Get samples from all admin unit 1 districts in SSA
setwd("C:/Users/bclamber/Dropbox/Weather")
interventions <- read.csv('Intervention_coverage.csv')
lCountries <- filter(interventions, CONTINENT=="Africa") %>%
  select(NAME_0) %>%
  unique()
lCountries <- lCountries$NAME_0
lCountries[!lCountries%in%shape$NAME_0]
# shape$NAME_0[str_detect(shape$NAME_0, "Tanz")]
lCountries <- as.character(lCountries)
lCountries[lCountries=="Congo (Brazzaville)"] <- "Republic of Congo"
lCountries[lCountries=="Congo, Democratic Republic of the"] <- "Democratic Republic of the Congo"
lCountries[lCountries=="Cote d'Ivoire"] <- "Côte d'Ivoire"
lCountries[lCountries=="Tanzania, United Republic of"] <- "Tanzania"
lCountries[!lCountries%in%shape$NAME_0]


lOverallSamples <- vector(mode="list")
for(j in seq_along(lCountries)){
  a_country_df <- shape[shape$NAME_0==lCountries[j], ]
  lNames <- unique(a_country_df$NAME_1)
  lSamples <- vector(mode="list")
  a_num_samples_per_unit_area <- 10
  for(i in seq_along(lNames)){
    a_admin1 <- a_country_df[a_country_df$NAME_1==lNames[[i]], ]
    tot_area <- sum(map_dbl(a_admin1@polygons, ~.@area))
    a_num_samples <- round(tot_area * a_num_samples_per_unit_area)
    a_num_samples <- if_else(a_num_samples>1, a_num_samples, 1)
    lSamples[[i]] <- spsample(a_admin1, a_num_samples, type="random")
  }
  names(lSamples) <- lNames
  lOverallSamples[[j]] <- lSamples
}
names(lOverallSamples) <- lCountries
saveRDS(lOverallSamples, file = 'africa_latlon.rds')

for(i in seq_along(lCountries)){
  if(i==1)
    lTest <- do.call(rbind, lOverallSamples[[i]])
  else
    lTest <- rbind(lTest, do.call(rbind, lOverallSamples[[i]]))
}

plot(lTest)
length(lTest)

# Number of sample points per country
lNumSamples <- vector(length = length(lCountries))
for(i in seq_along(lCountries)){
  lTest <- do.call(rbind, lOverallSamples[[i]])
  lNumSamples[i] <- length(lTest)
}

tibble(num_samples=lNumSamples, country=lCountries) %>%
  ggplot(aes(x=country, y=num_samples)) +
  geom_bar(stat="identity") +
  coord_flip()
