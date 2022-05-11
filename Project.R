# Project
# SK 03/23/22

# Determine operating system
OS <- .Platform$OS.type
# Set path for operating system
if (OS == "unix") {
  path <- "/Volumes/class/GEOG331_S22/students/sknopp/data/Project/" # MAC file path
}else if (OS == "windows") {
  path <- "Z:/GEOG331_S22/students/sknopp/data/Project" # windows file path
} else {
  print("ERROR: OS could not be identified")
}
setwd(path)

#load in lubridate
library(lubridate)
# load the terra package
library(terra)

##########################
### Method Definitions ###
##########################
# method to calculate doys for a particular weather station from the R dataframe
calcDoys <- function(stationframe) {
  doys = list()
  count <- 1
  for(i in 1:nrow(stationframe)){ #Iterate over entire dataframe
    if(stationframe$MONTH[i] == 1 && stationframe$DAY[i] == 1) {
      count <- 1
    }
    doys[[i]] = count
    count <- count + 1
  }
  return(unlist(doys))
}

# method to calculate a decimal year, and account for leap year
calcDecYear <- function(stationframe) {
  stationframe$decYear <- ifelse(leap_year(stationframe$YEAR), 
    stationframe$YEAR + (stationframe$doy/366), stationframe$YEAR + (stationframe$doy/365))
}

# method to prepare external data sets
prepData <- function(filename) {
  # read in data from station
  dat <- read.csv(filename)
  
  # quality control stationdat
  dat$PRECIPITATION <- ifelse(dat$PRECIPITATION == -99.99, NA, dat$PRECIPITATION)
  dat$MAX.TEMP <- ifelse(dat$MAX.TEMP == -99.9, NA, dat$MAX.TEMP)
  dat$MIN.TEMP <- ifelse(dat$MIN.TEMP == -99.9, NA, dat$MIN.TEMP)
  dat$MEAN.TEMP <- ifelse(dat$MEAN.TEMP == -99.9, NA, dat$MEAN.TEMP)
  
  # calculate doys for station data
  dat$doy<-calcDoys(dat)
  
  # calculate decimal year for station data
  dat$decYear <- calcDecYear(dat)
  
  return(dat)
}

# method to clip station data to event range based on year, lower, and upper
clip <- function(stationframe, year, lower, upper) {
  dat <- data.frame(stationframe$PRECIPITATION[stationframe$YEAR == year & stationframe$doy > lower & stationframe$doy < upper],
    stationframe$MEAN.TEMP[stationframe$YEAR == year & stationframe$doy > lower & stationframe$doy < upper],
    stationframe$decYear[stationframe$YEAR == year & stationframe$doy > lower & stationframe$doy < upper],
    stationframe$doy[stationframe$YEAR == year & stationframe$doy > lower & stationframe$doy < upper])
  colnames(dat) <- c("PRECIPITATION","MEAN.TEMP","decYear","doy")
  return(dat)
}



