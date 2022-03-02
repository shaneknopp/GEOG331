# GEOG 331 Activity 3
# SK 02/21/22

# Determine operating system
OS <- .Platform$OS.type
# Set path for operating system
if (OS == "unix") {
  path <- "/Volumes/class/GEOG331_S22/students/sknopp/data/bewkes/" # MAC file path
}else if (OS == "windows") {
  path <- "Z:/GEOG331_S22/students/sknopp/data/bewkes" # windows file path
} else {
  print("ERROR: OS could not be identified")
}
setwd(path)

##################
#    Example     #
##################
# create a function. Arguments in parentheses. Everything in curly brackets is definition.
assert <- function(statement,err.message) {
  # if evaluates if a statement is true or false for a single item
  if(statement == FALSE) {
    print(err.message)
  }
}


##################
#   Question 3   #
##################
# read in the data file
# skip the first 3 rows since there is additional column info
# specify the the NA is designated differently
datW <- read.csv("bewkes_weather.csv", na.strings=c("#N/A"), skip=3, header=FALSE)
# preview data
print(datW[1,])

# get sensor info from file
# this data table will contain all relevant units
sensorInfo <-   read.csv("bewkes_weather.csv", na.strings=c("#N/A"), nrows=2)
print(sensorInfo)
# get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
print(datW[1,])


##################
#   Question 4   #
##################
# install lubridate library
# install.packages(c("lubridate"))
# load lubridate add-on package
library(lubridate)

# convert to standardized format
# date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")
# calculate day of year
datW$doy <- yday(dates)
# calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
# calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
# quick preview of new date calcualtions
datW[1,]

# see how many values have missing data for each sensor observation
# air temperature
length(which(is.na(datW$air.temperature)))
# wind speed
length(which(is.na(datW$wind.speed)))
# precipitation
length(which(is.na(datW$precipitation)))
# soil temperature
length(which(is.na(datW$soil.moisture)))
# soil moisture
length(which(is.na(datW$soil.temp)))

# make a plot with filled in points (using pch)
# plot of soil moisture
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")
# plot of air temperature
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

# check the values at the extreme range of the data
# and throughout the percentiles
quantile(datW$air.tempQ1)

# look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  
# look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]


##################
#   Question 5   #
##################
# normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy

# make the plot with precipitation and lightning activity marked
# make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
# plot precipitation points only when there is precipitation 
# make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        
# plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

# determine if lightscale was modified uniformly
assert(length(c(datW$lightning.acvitivy)) == length(lightscale), "error: The parameters are not the same size")


##################
#   Question 6   #
##################
# filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
# create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))
# filter out wind speeds due to potential storms
datW$wind.speedQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                            ifelse(datW$precipitation > 5, NA, datW$wind.speed))

# Determine if speedQ2 and tempQ2 were modified uniformly
assert(sum(is.na(datW$wind.speedQ2)) == sum(is.na(datW$air.tempQ2)), "error: The vectors do not have the same number of NA elements")

# Generate a plot for new wind speed
plot(datW$DD , datW$wind.speedQ2, xlab = "Day of Year", ylab = "Wind Speed",
     type="n")
# plot wind speed points only when there is wind speed 
# make the points semi-transparent
points(datW$DD[datW$wind.speedQ2 > 0], datW$wind.speedQ2[datW$wind.speedQ2 > 0],
       col= "darkgoldenrod2", pch=15) 
# plot line only when there is wind speed
lines(datW$DD[datW$wind.speedQ2 > 0], datW$wind.speedQ2[datW$wind.speedQ2 > 0],
      col= "blue", pch=15)


##################
#   Question 7   #
##################
par(mfrow=c(2,2))

# plot soil moisture
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")
# plot soil temperature
plot(datW$DD, datW$soil.temp, pch = 19, type="b",
     xlab = "Day of Year", ylab = "Soil temperature (degrees C)")
# plot precipitation
plot(datW$DD, datW$precipitation, pch = 19, type="b",
     xlab = "Day of Year", ylab = "Precipitation (mm)")
# plot air temperature
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

# save plot to variable (used later)
plots <- recordPlot()

# Store soil moisture measurements for days leading up to outage (after 190)
datW$soil.moistureQ1 <- ifelse(datW$doy < 190, NA, datW$soil.moisture)
# Display the measurements without NA values
datW$soil.moistureQ1[!is.na(datW$soil.moistureQ1)]
# Look at distribution
quantile(datW$soil.moistureQ1, na.rm = TRUE)

# Store soil temperature measurements for days leading up to outage (after 190)
datW$soil.tempQ1 <- ifelse(datW$doy < 190, NA, datW$soil.temp)
datW$soil.tempQ1[!is.na(datW$soil.tempQ1)]
quantile(datW$soil.tempQ1, na.rm = TRUE)


##################
#   Question 8   #
##################
# Create a results table containing average air temperature 
results <- data.frame("avgAirTemp" = round(mean(datW$air.temperature, na.rm = TRUE), digits = 1)) #resolution 0.1C
# Add average wind speed to the table
results$avgWindSpeed <- round(mean(datW$wind.speed, na.rm = TRUE), digits = 2) #resolution 0.01 m/s
# Add average soil moisture
results$avgSoilMoisture <- round(mean(datW$soil.moisture, na.rm = TRUE), digits =  4) #resolution 0.0008 m3/m3
# Add average soil temperature
results$avgSoilTemp <- round(mean(datW$soil.temp, na.rm = TRUE), digits = 1) #resolution 0.1C
# Add average total precipitation
results$totPrecip <-round(sum(datW$precipitation, na.rm = TRUE), digits = 3) #resolution .017mm
# Add total number of observations using using time stamps
results$observations <- length(datW$timestamp)
# Add the time period of measurements
results$timePeriod <- max(datW$DD, na.rm = TRUE)
# display results table
results


##################
#   Question 9   #
##################
# display plots from variable (used earlier)
plots