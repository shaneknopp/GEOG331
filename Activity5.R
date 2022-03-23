# GEOG 331 Activity 5
# SK 03/23/22

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
