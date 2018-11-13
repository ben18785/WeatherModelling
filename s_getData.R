# Get climatic data ---------
rm(list=ls())
setwd("~/Dropbox/Malaria/Data_analysis/Meta_analysis_AnophelesAedesCulex/Combined/Temperature/")
library(ncdf4)
aTest <- nc_open('simple1')
aData <-ncvar_get(aTest,'t2m')
plot(ncvar_get(aTest,'latitude'))
plot(ncvar_get(aTest,'longitude'))
plot(ncvar_get(aTest,'time'))

lLat <- ncvar_get(aTest,'latitude')
lLon <- ncvar_get(aTest,'longitude')


fGetMeanRainfall <- function(aName,aNumber){
  aTest <- nc_open(paste0(aName,aNumber))
  aData <-ncvar_get(aTest,'tp')
  return(mean(aData))
}

fGetMeanTemperature <- function(aName,aNumber){
  aTest <- nc_open(paste0(aName,aNumber))
  aData <-ncvar_get(aTest,'t2m')
  return(mean(aData))
}

lTemp_simple <- sapply(seq(0,86,1),function(i) fGetMeanTemperature('simple',i)-273)
lTemp_complex <- sapply(seq(0,30,1),function(i) fGetMeanTemperature('complex',i)-273)
hist(c(lTemp_complex,lTemp_simple),20)
lRain_simple <- sapply(seq(0,86,1),function(i) fGetMeanRainfall('simple',i))
lRain_complex <- sapply(seq(0,30,1),function(i) fGetMeanRainfall('complex',i))

# Getting MRR_IDs -------
lTimeSeries <- read.csv('geoTime.csv')
main <- read.csv('MMRRdata_main.csv')
lTimeSeries$date <- main$date_st[match(lTimeSeries$mrr_id,main$MRR_ID)]
lLatLonDate <- lTimeSeries[,c(1:2,4)]
lLatLonDate <- unique(lLatLonDate)
lLatLon <- unique(lLatLonDate[,1:2])
lLatLonDate_nonUnique <- lTimeSeries[,c(1:2,4)]

aLen <- dim(lLatLon)[1]
lDates <- lapply(seq(1,aLen,1), function(i) droplevels(subset(lLatLonDate$date,lLatLonDate$latitude==lLatLon$latitude[i]&lLatLonDate$longitude[i])))
lDates <- lapply(seq(1,aLen,1), function(i) levels(lDates[[i]]))
lLatLon$Dates <- lDates

lDateTime <- strptime(as.character(lLatLonDate$date),"%d/%m/%Y %H:%M")
lDateTime_nonUnique <- strptime(as.character(lLatLonDate_nonUnique$date),"%d/%m/%Y %H:%M")
lDateTime <- as.Date(lDateTime)
lYear <- as.numeric(format(lDateTime,"%Y"))
lMonth <- as.numeric(format(lDateTime,"%m"))
lDay <- as.numeric(format(lDateTime,"%d"))
lYear_nonUnique <- as.numeric(format(lDateTime_nonUnique,"%Y"))
hist(lDay)
sum(lYear_nonUnique<1979)/length(lYear_nonUnique)
lFakeAverage <- lYear<1979
lNewYear <- ifelse(lFakeAverage,1979,lYear)
lNewMonthYear <- data.frame(fake=lFakeAverage,month=lMonth,year=lNewYear)
lLatLonDate <- unique(lLatLonDate)
lLatLon <- unique(lLatLonDate[,1:2])
lNewMonthYear$latitude <- lLatLonDate$latitude
lNewMonthYear$longitude <- lLatLonDate$longitude

lSimple <- lNewMonthYear[!lFakeAverage,]
lSimple1 <- unique(lSimple)
lComplex <- lNewMonthYear[lFakeAverage,]
lComplex1 <- unique(lComplex)
lSimple1$running <- seq(1,87,1)
lComplex1$running <- seq(1,31,1)

lNewMonthYear <- data.frame(fake=lFakeAverage,month=lMonth,year=lNewYear,day=lDay,oldYear=lYear)

## Matching up!
lTimeSeriesDateTime <- as.Date(strptime(as.character(lTimeSeries$date),"%d/%m/%Y %H:%M"))
lTimeSeries$year <- as.numeric(format(lTimeSeriesDateTime,"%Y"))
lTimeSeries$month <- as.numeric(format(lTimeSeriesDateTime,"%m"))
lTimeSeries$day <- as.numeric(format(lTimeSeriesDateTime,"%d"))
lTimeSeries$catDate <- paste0(lTimeSeries$year,lTimeSeries$month,lTimeSeries$lat)
lTimeSeries$catLat <- paste0(lTimeSeries$month,lTimeSeries$lat,lTimeSeries$long)

lSimple1$catDate <- paste0(lSimple1$year,lSimple1$month,lSimple1$latitude)
lComplex1$catLat <- paste0(lComplex1$month,lComplex1$latitude,lComplex1$longitude)
lTimeSeries$running <- lSimple1$running[match(lTimeSeries$catDate,lSimple1$catDate)]
lTimeSeries$running1 <- lComplex1$running[match(lTimeSeries$catLat,lComplex1$catLat)]

# Combine climatic data with MRR_IDs ------------
lTimeSeries$temp_simple <- lTemp_simple[lTimeSeries$running]
lTimeSeries$temp_complex <- lTemp_complex[lTimeSeries$running1]
lTimeSeries$temp <- rowMeans(lTimeSeries[,c(12:13)],na.rm = T)
lTimeSeries$rain_simple <- lRain_simple[lTimeSeries$running]
lTimeSeries$rain_complex <- lRain_complex[lTimeSeries$running1]
lTimeSeries$rain <- rowMeans(lTimeSeries[,c(15:16)],na.rm = T)

lWeatherData <- lTimeSeries[,c(1:7,14,17)]
plot(lWeatherData$latitude,lWeatherData$temp)
save(file='lWeather.RDa',lWeatherData)
