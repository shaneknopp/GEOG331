# GEOG 331 Activity 2 practice script
# SK 01/31/22

# make a vector of tree heights
heights <- c(30, 41, 20, 22)

#convert heights to cm
heights_cm <- heights*100

#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)

datw <- read.csv("Z:\\GEOG331_S22\data\noaa_weather\2011124.csv")
