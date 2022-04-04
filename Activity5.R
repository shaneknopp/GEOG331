# GEOG 331 Activity 5
# SK 03/23/22

# Determine operating system
OS <- .Platform$OS.type
# Set path for operating system
if (OS == "unix") {
  path <- "/Volumes/class/GEOG331_S22/students/sknopp/data/streamflow/" # MAC file path
}else if (OS == "windows") {
  path <- "Z:/GEOG331_S22/students/sknopp/data/streamflow" # windows file path
} else {
  print("ERROR: OS could not be identified")
}
setwd(path)

#load in lubridate
library(lubridate)

#read in streamflow data
datH <- read.csv("stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH) 

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("2049867.csv")                            
head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)


##################
#   Question 2   #
##################
#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))  


##################
#   Question 4   #
##################
#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))


##################
#    Example     #
##################
#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     xlim=c(0,360),
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)


##################
#   Question 5   #
##################
lines(datD$discharge[datD$year == 2017], col="red")
axis(1, seq(0,360, by=30), #tick intervals
     lab=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec",""),
     las = 2)
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation", "2017 Streamflow"), lwd=c(2,NA),
       col=c("black",rgb(0.392, 0.584, 0.929,.2), "red"),
       pch=c(NA,15),)


##################
#   Question 7   #
##################
yearTracker <- 2007
countEntries <- 0 #Number of entries per day
currentDoy <- 1
days = list()
years = list()
listIndex <- 1
for(i in 1:nrow(datP)){ #Iterate over entire dataframe
  if(datP$year[i] != yearTracker) { #Update year
    yearTracker <- datP$year[i]
    currentDoy <- datP$doy[i]
  }
  if(datP$doy[i] == currentDoy) {#Multiple entries for one doy
    countEntries <- countEntries + 1
  }
  else {#No more entries for one doy, reset count
    currentDoy <- currentDoy + 1
    countEntries <- 0
    if(datP$doy[i] == currentDoy) {#Check for multiple entries
      countEntries <- countEntries + 1
    }
  }
  if(countEntries == 24){#Determine if full day
    days[[listIndex]] <- currentDoy
    years[[listIndex]] <- yearTracker
    listIndex <- listIndex + 1
  }
}
fullD <- data.frame(unlist(days), unlist(years))
colnames(fullD) <- c("fullDay","year")

#calculate a decimal year, but account for leap year
fullD$decYear <- ifelse(leap_year(fullD$year),fullD$year + (fullD$fullDay/366),
                        fullD$year + (fullD$fullDay/365))

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),)
for(i in fullD$decYear){
  abline(v=i, col="red")
}
legend("topright", "Full Precip Data", lwd=c(1,NA), col= "red", pch=c(NA,15),)


##################
#     Example    #
##################
#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

##################
#   Question 8   #
##################
#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 356 & datD$doy < 358 & datD$year == 2012,]
hydroP <- datP[datP$doy >= 356 & datP$doy < 358 & datP$year == 2012,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


##################
#    Example     #
##################
library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()
#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()


##################
#   Question 9   #
##################
datD$yearPlot <- as.factor(datD$year)
datD$month <- month(datesD)

#isloate 2016 into data frame
year16 <- data.frame(datD$discharge[datD$year==2016], datD$month[datD$year==2016])
colnames(year16) <- c("discharge", "month")

#isloate 2017 into data frame
year17 <- data.frame(datD$discharge[datD$year==2017], datD$month[datD$year==2017])
colnames(year17) <- c("discharge", "month")

#fully filter by season for every
#https://www.almanac.com/content/first-day-seasons
year16$season <- ifelse(year16$month < 3, "Winter",
                        ifelse(year16$month < 6, "Spring",
                               ifelse(year16$month < 9, "Summer",
                                      ifelse(year16$month <= 11, "Fall",
                                             ifelse(year16$month <= 12, "Winter")))))

year17$season <- ifelse(year17$month < 3, "Winter",
                        ifelse(year17$month < 6, "Spring",
                               ifelse(year17$month < 9, "Summer",
                                      ifelse(year17$month <= 11, "Fall",
                                             ifelse(year17$month <= 12, "Winter")))))

ggplot(data= year16, aes(x = season, y = discharge)) + 
  geom_violin() + 
  ggtitle("2016 Discharge by Season") +
  xlab("Seasons") +
  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1")))

ggplot(data= year17, aes(x = season, y = discharge)) + 
  geom_violin() + 
  ggtitle("2017 Discharge by Season") +
  xlab("Seasons") +
  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1")))