# GEOG 331 Activity 2
# SK 02/08/22

OS <- .Platform$OS.type

if (OS == "unix"){
  path <- "/Volumes/class/GEOG331_S22/students/sknopp/data/noaa_weather/" # MAC file path
} else if (OS == "windows"){
  path <- "Z:/GEOG331_S22/students/sknopp/data/noaa_weather" # windows file path
} else {
  print("ERROR: OS could not be identified")
}
setwd(path)

datW <- read.csv("2011124.csv")

##################
#   Question 1   #
##################
nrow(datW)
ncol(datW)


# specify a column with a proper date format
# note the format here dataframe$column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
# create a date column by reformatting the date to only include years
# and indicating that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))


##################
#   Question 2   #
##################
charvec <- c("One", "Two", "Three", "Four", "Five")
numvec <- c(1.5, 2.5, 3.5, 4.5, 5.5)
intvec <- c(1L, 2L, 3L, 4L, 5L)
facvec <- as.factor(c("a","b", "c", "d", "e"))


##################
#    Example     #
##################
# find out all unique site names
unique(datW$NAME)
# converts site names to factors
datW$NAME <- as.factor(datW$NAME)
levels(datW$NAME)
# look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])
# look at the mean maximum temperature for Aberdeen
# with na.rm argument set to true to ingnore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
# calculate the average daily temperature
# This temperature will be halfway between the minimum and maximum temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)
# get the mean across all sites
# the by function is a list of one or more variables to index over.
# FUN indicates the function we want to use
# if you want to specify any function specific arguments use a comma and add them after the function
# here we want to use the na.rm arguments specific to 
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp
# change the automatic output of column names to be more meaningful
# note that MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp
# convert level to number for factor data type
# you will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)


##################
#   Question 3   #
##################
# make a histogram for the first site in our levels, Aberdeen
# main= is the title name argument.
# Here you want to paste the actual name of the factor not the numeric index
# since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
help(hist)
help(paste)

# add mean line with red (tomato3) color
# and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
# add standard deviation line below the mean with red (tomato3) color
# and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
# add standard deviation line above the mean with red (tomato3) color
# and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)


##################
#   Question 4   #
##################
par(mfrow=c(2, 2))
colors <- c("midnightblue", "darkgoldenrod1", "mediumaquamarine", "coral3")
for(x in 2:5) {
  # random hex color generator
  # hex <- paste(sample(c(0:9, LETTERS[1:6]), 6, T), collapse='')
  hist(datW$TAVE[datW$siteN == x],
       freq=FALSE,
       main = paste(levels(datW$NAME)[x]),
       xlab = "Average daily temperature (degrees C)",
       ylab="Relative frequency",
       # concatenate the hex color with pound sign (not used for visual appeal)
       # col=paste("#",hex,sep=""),
       col=colors[x-1],
       border="white")
  abline(v = mean(datW$TAVE[datW$siteN == x],na.rm=TRUE),
         col = "tomato3",
         lwd = 3)
  abline(v = mean(datW$TAVE[datW$siteN == x],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == x],na.rm=TRUE),
         col = "tomato3",
         lty = 3,
         lwd = 3)
  abline(v = mean(datW$TAVE[datW$siteN == x],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == x],na.rm=TRUE),
         col = "tomato3",
         lty = 3,
         lwd = 3)
}


##################
#   Question 5   #
##################
par(mfrow=c(1, 1))
# make a histogram for the first site in our levels
# main= is the title name argument.
# Here you want to paste the actual name of the factor not the numeric index
# since that will be more meaningful. 
# note I've named the histogram so I can reference it later
h1 <- hist(datW$TAVE[datW$siteN == 1],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")
# the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot <- seq(-10,30, length.out = 100)
# the dnorm function will produce the probability density based on a mean and standard deviation.
y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
# create a density that is scaled to fit in the plot  since the density has a different range from the data density.
# !!! this is helpful for putting multiple things on the same plot
# !!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two datasets on the plot. Here both plots share zero as a minimum.
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot
# points function adds points or lines to a graph  
# the first two arguements are the x coordinates and the y coordinates.
points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)


##################
#    Example     #
##################
help(dnorm)
# pnorm(value to evaluate at (note this will evaluate for all values and below),mean, standard deviation)
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
# pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
# pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))- pnorm(0,
                                                        mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                                                        sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
# pnorm of 20 gives me all probability (area of the curve) below 20 
# subtracting from one leaves me with the area above 20
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#pnorm of 20 gives me all probability (area of the curve) below 20 
#subtracting from one leaves me with the area above 20
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))


##################
#   Question 6   #
##################
# Increase of 4C
1 - pnorm(20,
          mean((datW$TAVE[datW$siteN == 1] + 4.0),na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
qnorm(0.95,
      mean((datW$TAVE[datW$siteN == 1] + 4.0),na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))


##################
#   Question 7   #
##################
hist(datW$PRCP[datW$siteN == 1],
     freq = FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Precipitation (mm)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")


##################
#   Question 8   #
##################
annualPrecip <- aggregate(datW$PRCP, by = list(datW$year, datW$siteN), FUN="sum" , na.rm=TRUE)
colnames(annualPrecip)<-c("Year", "Site", "Precip")
hist(annualPrecip$Precip[annualPrecip$Site == 1],
     freq = FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Annual Precipitation (mm)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")


##################
#   Question 9   #
##################
aggregate(annualPrecip$Precip, by=list(annualPrecip$Site), FUN="mean",na.rm=TRUE)

