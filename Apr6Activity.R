# GEOG 331 Class Activity 2
# SK 04/06/22

# load the terra package
library(terra)

# Determine operating system
OS <- .Platform$OS.type
# Set path for operating system
if (OS == "unix") {
  path <- "/Volumes/class/GEOG331_S22/students/sknopp/" # MAC file path
}else if (OS == "windows") {
  path <- "Z:/GEOG331_S22/students/sknopp" # windows file path
} else {
  print("ERROR: OS could not be identified")
}

# set working directory
setwd(path)

# read a raster from file
p <- rast("/Volumes/class/GEOG331_S22/data/rs_data/20190706_002918_101b_3B_AnalyticMS_SR.tif")

# plot the data
plot(p)

# plot an rgb rendering of the data (assumes raster is an 8 bit image)
plotRGB(p, r = 3, g = 2, b = 1)

# plot an rgb rendering of the data (sets rgb to process 16 bit image)
plotRGB(p, r = 3, g = 2, b = 1,
        scale = 65535,
        stretch = "hist")

# reproject the first layer to polar stereigraphic projection
