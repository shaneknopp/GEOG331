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

#---------- planning to make this in a method & for loop since I will be using data from multiple stationa ------------
#read in data from Melbourne station
datM <- read.csv("85612_2010_1_1_2021.csv")
datM$PRECIPITATION <- ifelse(datM$PRECIPITATION == -99.99, NA, datM$PRECIPITATION)
datM$MAX.TEMP <- ifelse(datM$MAX.TEMP == -99.9, NA, datM$MAX.TEMP)
datM$MIN.TEMP <- ifelse(datM$MIN.TEMP == -99.9, NA, datM$MIN.TEMP)
datM$MEAN.TEMP <- ifelse(datM$MEAN.TEMP == -99.9, NA, datM$MEAN.TEMP)

doys = list()
count <- 1
for(i in 1:nrow(datM)){ #Iterate over entire dataframe
  if(datM$MONTH[i] == 1 && datM$DAY[i] == 1) {
    count <- 1
  }
  doys[[i]] = count
  count <- count + 1
}
datM$doy <- unlist(doys)

#calculate a decimal year, but account for leap year
datM$decYear <- ifelse(leap_year(datM$YEAR),datM$YEAR + (datM$doy/366),
                       datM$YEAR + (datM$doy/365))

#----------------------------------------------------------------------
df2020 <- data.frame(datM$PRECIPITATION[datM$YEAR == 2020], datM$MEAN.TEMP[datM$YEAR == 2020], datM$decYear[datM$YEAR == 2020])
colnames(df2020) <- c("PRECIPITATION","MEAN.TEMP","decYear")

decday <- ifelse(leap_year(2020),2020 + (252/366),
                       2020 + (252/365))
#plot precipitation
plot(df2020$decYear, df2020$PRECIPITATION, type="l", scale.frac = 0.6,main="", xlab="",ylab="",axes=TRUE)
mtext("Decimal Day in 2020", side=1, line=3, font=2)
mtext("Precipitation (in)", side=2, line=3, font=2)
plot(df2020$decYear, df2020$PRECIPITATION, type="l", xlab=expression(paste("Decimal Day in 2020")), ylab=expression(paste("Precipitation (in)")),)
points(2020.689, df2020$PRECIPITATION[252], col="red")

#plot min temperature
#plot(datM$decYear, datM$MIN.TEMP, type="l", xlab="Year", ylab="Min Temperature (fahrenheit)",)

#plot max temperature
#plot(datM$decYear, datM$MAX.TEMP, type="l", xlab="Year", ylab="Max Temperature (fahrenheit)",)

#plot mean temperature
plot(df2020$decYear, df2020$MEAN.TEMP, type="l", xlab=expression(paste("Decimal Day in 2020")), ylab=expression(paste("Mean Temperature (°F)")),)
points(2020.689, df2020$MEAN.TEMP[252], col="red")

par(mfrow=c(1,1))
p <- rast("L2020252.L3m_DAY_CYAN_CI_cyano_CYAN_CONUS_300m_7_5.tif")
plot(p)



#read in data from Royal Palm Beach station
datRPB <- read.csv("87760_2010_1_1_2021.csv")
#read in data from West Palm Beach station
datRPB <- read.csv("89525_2010_1_1_2021.csv")