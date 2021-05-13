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


bathy <- raster('/home/anitag/MBH_Ningaloo_Parks/rasters/depth-Ningaloo-Parks-MBH.tif')
plot(bathy)


## read legacy ---

lega <- readOGR(paste(s.dir, "Points_from_Steve.shp", sep='/'))
plot(lega, add=T)

# crop domain to Pt Cloates and Legacy 
e <- drawExtent()

b <- crop(bathy, e)
plot(b)
plot(lega, pch = 20, add=T)

values(b)[values(b) < -400] = NA
values(b)[values(b) > -200] = NA

plot(b)
plot(lega, pch = 20, add=T)

writeRaster(b, paste(r.dir, "depth_200-400m_Ningaloo.tif", sep='/'), overwrite = T)

s <- terrain(b, 'slope')
plot(s, main = "slope")

t <- terrain(b, 'TPI')
plot(t, main='tpi')

covariates <- stack(b, s, t)
covariates

names(covariates) <- c("depth", "slope", "tpi")
writeRaster(covariates, paste(r.dir, "Ningaloo_predictors.tif", sep='/'))
