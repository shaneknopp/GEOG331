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
  
  # quality control station data
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


##########################
### Melbourne Analysis ###
##########################
# prep data from the Melbourne Station
datM <- prepData("85612_2010_1_1_2021.csv")

# Melbourne Station statistics
fit <- lm(datM$PRECIPITATION~datM$MEAN.TEMP)
summary(fit)
plot(datM$MEAN.TEMP,datM$PRECIPITATION)
abline(fit)
hist(summary(fit$residuals))
shapiro.test(summary(fit)$residuals)
qqnorm(summary(fit)$residuals, pch = 16) 
qqline(summary(fit)$residuals, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7, pch = 16)

### September 9, 2020 ###
# create a data frame just of 2020 Melbourne data for plots
Melbourne2020 <- data.frame(datM$PRECIPITATION[datM$YEAR == 2020], datM$MEAN.TEMP[datM$YEAR == 2020], datM$decYear[datM$YEAR == 2020])
# rename data frame columns
colnames(Melbourne2020) <- c("PRECIPITATION","MEAN.TEMP","decYear")

# create a data frame with data 3 days before and after event
Melbourne252week <- clip(datM, 2020, 248, 256)
Melbourne252week

#plot precipitation
plot(Melbourne2020$decYear, Melbourne2020$PRECIPITATION, type="l", xlab=expression(paste("Decimal Day in 2020")), ylab=expression(paste("Precipitation (in)")),)
points(2020.689, Melbourne2020$PRECIPITATION[252], col="red")

#plot mean temperature
plot(Melbourne2020$decYear, Melbourne2020$MEAN.TEMP, type="l", xlab=expression(paste("Decimal Day in 2020")), ylab=expression(paste("Mean Temperature (°F)")),)
points(2020.689, Melbourne2020$MEAN.TEMP[252], col="red")

# Load the algal bloom raster image
Melbourne252 <- rast("L2020252.L3m_DAY_CYAN_CI_cyano_CYAN_CONUS_300m_7_5.tif")
# Plot the algal bloom raster image
plot(Melbourne252)

# Crop to Melbourne
Melbourne252cropped <- crop(Melbourne252, ext(1460000,1600000, 650000, 800000))

# Reclassify vector values
reclass_v <- c(0, 0, 0,
               0, 253, 1,
               254, 254, 2,
               255, 255, 3)

# Convert vector to matrix
reclass_m <- matrix(reclass_v,
                    ncol = 3,
                    byrow = TRUE)

# 
Melbourne252reclassified <- classify(Melbourne252cropped, reclass_m)

first_column <- c("0","1","254","255")
second_column <- c("Inconclusive","Algal Bloom","Land","No Data")
colors <-c("#fc8d62","#8da0cb","#a6d854", "#bcbcbc")
df <- data.frame(first_column, second_column, colors)
plot(Melbourne252reclassified, col = df$colors, legend = FALSE, axes = FALSE)
legend("bottom", paste(df$second_column),
       fill=df$colors ,bty="n",horiz = T) 

terra::freq(Melbourne252reclassified)

### June 25, 2019 ###
Melbourne2019 <- data.frame(datM$PRECIPITATION[datM$YEAR == 2019], datM$MEAN.TEMP[datM$YEAR == 2019], datM$decYear[datM$YEAR == 2019])
colnames(Melbourne2019) <- c("PRECIPITATION","MEAN.TEMP","decYear")

Melbourne176week <- clip(datM, 2019, 172, 180)
Melbourne176week

plot(Melbourne2019$decYear, Melbourne2019$PRECIPITATION, type="l", xlab=expression(paste("Decimal Day in 2019")), ylab=expression(paste("Precipitation (in)")),)
points(2019.482, Melbourne2019$PRECIPITATION[176], col="red")

plot(Melbourne2019$decYear, Melbourne2019$MEAN.TEMP, type="l", xlab=expression(paste("Decimal Day in 2019")), ylab=expression(paste("Mean Temperature (°F)")),)
points(2019.482, Melbourne2019$MEAN.TEMP[176], col="red")
Melbourne176 <- rast("L2019176.L3m_DAY_CYAN_CI_cyano_CYAN_CONUS_300m_7_5.tif")
plot(Melbourne176)


############################
### Florida Bay Analysis ###
############################
# prep data from the Everglades Station
datE <- prepData("82850_2000_1_1_2017.csv")

# Everglades Station statistics
fit <- lm(datE$PRECIPITATION~datE$MEAN.TEMP)
summary(fit)
plot(datE$MEAN.TEMP,datE$PRECIPITATION)
abline(fit)
hist(summary(fit$residuals))
shapiro.test(summary(fit)$residuals)
qqnorm(summary(fit)$residuals, pch = 16) 
qqline(summary(fit)$residuals, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7, pch = 16)

### January 18, 2012 ###
Everglades2012 <- data.frame(datM$PRECIPITATION[datM$YEAR == 2012], datM$MEAN.TEMP[datM$YEAR == 2012], datM$decYear[datM$YEAR == 2012])
colnames(Everglades2012) <- c("PRECIPITATION","MEAN.TEMP","decYear")

Everglades018week <- clip(datM, 2012, 14, 22)
Everglades018week

plot(Everglades2012$decYear, Everglades2012$PRECIPITATION, type="l", xlab=expression(paste("Decimal Day in 2012")), ylab=expression(paste("Precipitation (in)")),)
points(2012.049, Everglades2012$PRECIPITATION[18], col="red")

plot(Everglades2012$decYear, Everglades2012$MEAN.TEMP, type="l", xlab=expression(paste("Decimal Day in 2012")), ylab=expression(paste("Mean Temperature (°F)")),)
points(2012.049, Everglades2012$MEAN.TEMP[18], col="red")
Everglades018 <- rast("M2012018.L3m_DAY_CYAN_CI_cyano_CYAN_CONUS_300m_7_5.tif")
plot(Everglades018)

### July 7, 2016 ###
Everglades2016 <- data.frame(datM$PRECIPITATION[datM$YEAR == 2016], datM$MEAN.TEMP[datM$YEAR == 2016], datM$decYear[datM$YEAR == 2016])
colnames(Everglades2016) <- c("PRECIPITATION","MEAN.TEMP","decYear")

Everglades018week <- clip(datM, 2016, 185, 193)
Everglades018week

plot(Everglades2016$decYear, Everglades2016$PRECIPITATION, type="l", xlab=expression(paste("Decimal Day in 2016")), ylab=expression(paste("Precipitation (in)")),)
points(2016.516, Everglades2016$PRECIPITATION[189], col="red")

plot(Everglades2016$decYear, Everglades2016$MEAN.TEMP, type="l", xlab=expression(paste("Decimal Day in 2016")), ylab=expression(paste("Mean Temperature (°F)")),)
points(2016.516, Everglades2016$MEAN.TEMP[189], col="red")
Everglades189 <- rast("L2016189.L3m_DAY_CYAN_CI_cyano_CYAN_CONUS_300m_7_5.tif")
plot(Everglades189)


############################
### Canal Point Analysis ###
############################
# prep data from the Canal Point Station
datCP <- prepData("81276_2010_1_1_2021.csv")
# extra quality control only needed for CP station
datCP$PRECIPITATION <- ifelse(datCP$PRECIPITATION == -99.9, NA, datCP$PRECIPITATION)
datCP$MIN.TEMP <- ifelse(datCP$MIN.TEMP == -99.99, NA, datCP$MIN.TEMP)
datCP$MEAN.TEMP <- ifelse(datCP$MEAN.TEMP == -99.99, NA, datCP$MEAN.TEMP)

# Okeechobee Station statistics
fit <- lm(datCP$PRECIPITATION~datCP$MEAN.TEMP)
summary(fit)
plot(datCP$MEAN.TEMP,datCP$PRECIPITATION)
abline(fit)
hist(summary(fit$residuals))
shapiro.test(summary(fit)$residuals)
qqnorm(summary(fit)$residuals, pch = 16) 
qqline(summary(fit)$residuals, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7, pch = 16)

### June 2, 2019 ###
CP2019 <- data.frame(datM$PRECIPITATION[datM$YEAR == 2019], datM$MEAN.TEMP[datM$YEAR == 2019], datM$decYear[datM$YEAR == 2019])
colnames(CP2019) <- c("PRECIPITATION","MEAN.TEMP","decYear")

CP153week <- clip(datM, 2019, 149, 157)
CP153week

plot(CP2019$decYear, CP2019$PRECIPITATION, type="l", xlab=expression(paste("Decimal Day in 2019")), ylab=expression(paste("Precipitation (in)")),)
points(2019.419, CP2019$PRECIPITATION[153], col="red")

plot(CP2019$decYear, CP2019$MEAN.TEMP, type="l", xlab=expression(paste("Decimal Day in 2019")), ylab=expression(paste("Mean Temperature (°F)")),)
points(2019.419, CP2019$MEAN.TEMP[153], col="red")
CP153 <- rast("L2019153.L3m_DAY_CYAN_CI_cyano_CYAN_CONUS_300m_7_5.tif")
plot(CP153)

### July 22, 2011 ###
CP2011 <- data.frame(datM$PRECIPITATION[datM$YEAR == 2011], datM$MEAN.TEMP[datM$YEAR == 2011], datM$decYear[datM$YEAR == 2011])
colnames(CP2011) <- c("PRECIPITATION","MEAN.TEMP","decYear")

CP203week <- clip(datM, 2011, 199, 207)
CP203week

plot(CP2011$decYear, CP2011$PRECIPITATION, type="l", xlab=expression(paste("Decimal Day in 2011")), ylab=expression(paste("Precipitation (in)")),)
points(2011.556, CP2011$PRECIPITATION[203], col="red")

plot(CP2011$decYear, CP2011$MEAN.TEMP, type="l", xlab=expression(paste("Decimal Day in 2011")), ylab=expression(paste("Mean Temperature (°F)")),)
points(2011.556, CP2011$MEAN.TEMP[203], col="red")
CP203 <- rast("M2011203.L3m_DAY_CYAN_CI_cyano_CYAN_CONUS_300m_7_5.tif")
plot(CP203)
