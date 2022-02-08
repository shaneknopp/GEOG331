# GEOG 331 Activity 2
# SK 02/08/22

OS <- .Platform$OS.type

if (OS == "unix"){
  temp_path <- "/Volumes/class/GEOG331_S22/students/sknopp/data/noaa_weather/" # MAC file path
} else if (OS == "windows"){
  temp_path <- "Z:/GEOG331_S22/students/sknopp/data/noaa_weather" # windows file path
} else {
  print("ERROR: OS could not be identified")
}
setwd(temp_path)

#Question 1
datW <- read.csv("2011124.csv")
nrow(datW)
ncol(datW)

#specify a column with a proper date format
#note the format here dataframe$column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
#google date formatting in r to find more options and learn more

#create a date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))

#Question 2
charvec <- c("One", "Two", "Three", "Four", "Five")
numvec <- c(1.5, 2.5, 3.5, 4.5, 5.5)
intvec <- c(1L, 2L, 3L, 4L, 5L)
facvec
