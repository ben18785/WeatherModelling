rm(list=ls())
setwd("~/Dropbox/Malaria/Data_analysis/Meta_analysis_AnophelesAedesCulex/Combined/Temperature")

# Load data -----
main <- read.csv('MMRRdata_main.csv')
main$long[91]
main$lat[91]
main$Country[91]

aLen <- dim(lLatLon)[1]
lDates <- lapply(seq(1,aLen,1), function(i) droplevels(subset(lLatLonDate$date,lLatLonDate$latitude==lLatLon$latitude[i]&lLatLonDate$longitude[i])))
lDates <- lapply(seq(1,aLen,1), function(i) levels(lDates[[i]]))
lLatLon$Dates <- lDates

lDateTime <- strptime(as.character(lLatLonDate$date),"%d/%m/%Y %H:%M")
lDateTime <- as.Date(lDateTime)
lYear <- as.numeric(format(lDateTime,"%Y"))
lMonth <- as.numeric(format(lDateTime,"%m"))
lDay <- as.numeric(format(lDateTime,"%d"))
hist(lDay)
sum(lYear<1979)
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

# Generate north, west, south and east for relevant areas
fAreaGenerator <- function(aDegree,aLatitude,aLongitude){
  north <- aLatitude + aDegree
  south <- aLatitude - aDegree
  west <- aLongitude - aDegree
  east <- aLongitude + aDegree
  return(list(north=north,west=west,south=south,east=east))
}

lArea_complex <- lapply(seq(1,dim(lComplex1)[1],1),function(i) fAreaGenerator(1,lComplex1$latitude[i],lComplex1$longitude[i]))
lArea_simple <- lapply(seq(1,dim(lSimple1)[1],1), function(i) fAreaGenerator(1,lSimple1$latitude[i],lSimple1$longitude[i]))
lArea_complex1 <- matrix(unlist(lArea_complex),nrow = dim(lComplex1)[1],byrow = T)
colnames(lArea_complex1) <- c("north","west","south","east")
lArea_simple1 <- matrix(unlist(lArea_simple),nrow = dim(lSimple1)[1],byrow = T)
colnames(lArea_simple1) <- c("north","west","south","east")
lArea_simple2 <- apply(lArea_simple1, 1, function(x) paste(x,collapse = "/"))
lArea_complex2 <- apply(lArea_complex1, 1, function(x) paste(x,collapse = "/"))
write.csv(file='lArea_complex.csv',lArea_complex2,row.names = F)
write.csv(file='lArea_simple.csv',lArea_simple2,row.names = F)

# Generate dates in form used by ecmwf
fGenerateDate <- function(aMonth,aYear){
  bMonth <- ifelse(aMonth<10,paste0(0,aMonth),aMonth)
  return(paste0(aYear,bMonth,"01"))
}

fGenerateDateComplex <- function(aNumYears,aMonth,aYear){
  bMonth <- ifelse(aMonth<10,paste0(0,aMonth),aMonth)
  lDates <- sapply(seq(0,aNumYears-1,1), function(i) paste0(aYear+i,bMonth,"01"))
  return(paste(lDates,collapse = "/"))
}

lDate_simple <- lapply(seq(1,dim(lSimple1)[1],1), function(i) fGenerateDate(lSimple1$month[i],lSimple1$year[i]))
lDate_complex <- lapply(seq(1,dim(lComplex1)[1],1), function(i) fGenerateDateComplex(3,lComplex1$month[i],lComplex1$year[i]))
write.csv(file='lDate_simple.csv',unlist(lDate_simple),row.names = F)
write.csv(file='lDate_complex.csv',unlist(lDate_complex),row.names = F)


