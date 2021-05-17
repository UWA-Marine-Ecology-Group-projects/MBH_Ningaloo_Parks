###   ###   ###    Prepare spatial data for analysis    ###   ###   ###

# Libraries ----
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
#library(pals)
library(RColorBrewer)


# clear environment ----
rm(list = ls())


# Directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "~/MBH_AbroNPZs"
o.dir <- paste(w.dir, "outputs", sep = '/')
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "shapefiles", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')

dirx <- "/home/anitag"


## Load bathy and park data ----

# load West coast bathy --
b <- raster(paste("rasters/Ningaloo-bathy-v5.tif", sep='/'))

plot(b)


# read shapefiles of zones ----
npin <- readOGR(paste(s.dir, "Point-Cloates-inNP.shp", sep='/'))
plot(npin, add=T)

npout <- readOGR(paste(s.dir, "Point-Cloates-outNPsouth.shp", sep='/'))
plot(npout, add=T)

# crop bathy to zones ----
e <- drawExtent()

b2 <- crop(b, e)
plot(b2)
plot(npin, add=T)
plot(npout, add=T)

# Get seafloor terrain ----

depth <- b2

slope <- terrain(depth, "slope")
plot(slope)

tpi <- terrain(depth, "TPI")
plot(tpi)

flowdir <- terrain(depth, "flowdir")
plot(flowdir)

roughness <- terrain(depth, "roughness")
plot(roughness)

aspect <- terrain(depth, "aspect")
plot(aspect)

sea.terrain <- stack(depth, slope, tpi, flowdir, roughness, aspect)
plot(sea.terrain)
names(sea.terrain) <- c("depth","slope", "tpi", "flowdir", "roughness", "aspect")

# save derivatives of sea terrain ----
writeRaster(sea.terrain, paste(r.dir, "PtCloates_sea-terrain-v5.tif", sep='/'), overwrite=T)




