# GEOG 331 Activity 3
# SK 02/21/22

OS <- .Platform$OS.type

if (OS == "unix"){
  path <- "/Volumes/class/GEOG331_S22/students/sknopp/data/bewkes/" # MAC file path
} else if (OS == "windows"){
  path <- "Z:/GEOG331_S22/students/sknopp/data/bewkes" # windows file path
} else {
  print("ERROR: OS could not be identified")
}
setwd(path)

datW <- read.csv("bewkes_weather.csv")
