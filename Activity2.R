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
datW <- read.csv("2011124.csv")

