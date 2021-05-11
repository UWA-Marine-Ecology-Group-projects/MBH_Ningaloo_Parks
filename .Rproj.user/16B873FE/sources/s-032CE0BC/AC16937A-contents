## explore bathy and legacy sites ##

# libraries ----
library( rgdal)
library( sp)
library( raster)


# clear environment ----
rm(list = ls())


# Set names ----
study <- "Ningaloo-Parks-MBH"


# Directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "~/MBH_AbroNPZs"
o.dir <- paste(w.dir, "outputs", sep = '/')
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "shapefiles", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')

# read bathy ----
b <- raster(paste(r.dir, "depth-Ningaloo-Parks-MBH.tif", sep='/'))
plot(b)
#b2 <- flip(b, 'y')
#plot(b2)
#b3 <- projectRaster(b2, crs=proj4string(legacy))
e <- drawExtent()
b <- crop(b, e)
plot(b)
b[b < -400] <- NA
plot(b, main = 'Depth and legacy')
writeRaster(b, paste(r.dir, paste0("depth-", study, ".tif"), sep='/'))

s <- terrain(b, "slope")
plot(s, main="Slope and legacy")
writeRaster(s, paste(r.dir, paste0("slope-", study, ".tif"), sep='/'))

t <- terrain(b, "TPI")
plot(t, main="TPI and legacy")
writeRaster(t, paste(r.dir, paste0("tpi-", study, ".tif"), sep='/'))


# read legacy sites ----
legacy <- readOGR(paste(s.dir, "Points_from_Steve.shp", sep='/'))
plot(legacy, add=T)


df <- extract(s, legacy, df=T)
min(df$slope)
max(df$slope)

hist(df$slope, main="slope distribution of Steve's points", xlab ='slope')
