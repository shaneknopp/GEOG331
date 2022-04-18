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

# read file with field observations of canopy cover
tree <- read.csv("/Volumes/class/GEOG331_S22/data/rs_data/siberia_stand_data.csv", header = T)
head(tree)

# convert to vector object using terra package
gtree <- vect(tree, geom = c("Long", "Lat"), "epsg:4326")

# project the data to match the coordinate system of the raster layer
gtree2 <- project(gtree,p)
plot(gtree2, add=T, col = "red")

# create a polygon from the extent of the points
b <- as.lines(ext(gtree), "epsg:4326")

# reproject the polygons to the same projection as our raster
b2 <- project(b,crs(p))

# buffer the extent by 200m
b3 <- buffer(b2, width = 200)

# use this to crop the raster layer so we
p2 <- crop(p, b3, filename = "20190706_SR_crop.tif", overwrite=TRUE)

# make a plot to see how it looks
plotRGB(p2, r = 3, g = 2, b = 1,
        scale = 65535,
        stretch = "lin")

points(gtree2, col="blue", cex = gtree2$cc.pct/50)

# calculate NDVI
ndvi <- (p2[[4]]-p2[[3]])/(p2[[4]]+p2[[3]])

# set the layer name to ndvi to avoid confusion
names(ndvi) <- "ndvi"

#create a plot of the ndvi map with sample points on top
png(filename = "ndvi_map.png", width = 6, height = 4, units = "in", res = 300)

dev.off()

plot(ndvi)
points(gtree2, cex = gtree$cc.pct/50, col = "blue")

# extract NDVI values for each point
nt <- terra::extract(ndvi, gtree2, fun = mean, method = 'bilinear')

# plot ndvi vs. canopy cover
plot(nt$ndvi,gtree2$cc.pct, pch = 16, col = "blue")

# plot again but fix the y-axis
plot(nt$ndvi,gtree2$cc.pct, pch = 16, col = "blue", xlim= c(0,1))


G = 2.5
c1 = 6
c2 = 7.5
