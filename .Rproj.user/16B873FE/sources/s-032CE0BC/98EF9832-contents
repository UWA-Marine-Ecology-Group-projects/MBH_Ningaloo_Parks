###   ###   ###   Read data for MBH analysis    ###   ###   ###



# libraries ----
library( rgdal)
library( sp)
library( raster)


# clear environment ----
rm(list = ls())



# Set names ----
study <- "PtCloates"



# Directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "~/MBH_AbroNPZs"
o.dir <- paste(w.dir, "outputs", sep = '/')
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "shapefiles", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')



# Read polygons ----
inNP <- readOGR(paste(s.dir, "Point-Cloates-inNP.shp", sep = '/'))
outNP <- readOGR(paste(s.dir, "Point-Cloates-outNP.shp", sep = '/'))


# check crs --
proj4string(inNP)
proj4string(outNP)



# Prepare list of polygons ----
zones <- list()
zones$inNP <- inNP
zones$outNP <- outNP


# join zones polygons
zones$All <- raster::union(zones$inNP, zones$outNP)

plot(zones$All)

head(zones$All)

#intial look to see area
plot( zones$All , border='black')
plot( zones$inNP , border='black', col = 'green', add=TRUE)
plot( zones$outNP, add=TRUE, col='blue')



# Save zones rds ----

saveRDS(zones, file= paste0(paste(d.dir, paste("Zones" , study, sep='_'), sep='/'), ".RDS"))


## Read raster data ----
ders <- stack(paste(r.dir, "PtCloates_sea-terrain.tif", sep ='/'))
plot(ders)
names(ders) <-  c("depth","slope", "tpi", "flowdir", "roughness", "aspect")
plot(ders)

ders <- mask(ders, zones$All)
plot(ders)

# Save rasters rds ----
rasterfiles <- list()
rasterfiles$depth <- ders$depth
rasterfiles$slope <- ders$slope
rasterfiles$tpi <- ders$tpi

# Save raster data ----
saveRDS(rasterfiles, file= paste0(paste(d.dir, paste("rasters" , study, sep='_'), sep='/'), ".RDS"))



## Converting polygons to a common raster ----
b <- ders$depth
plot(b)


###       ###       ### this takes a while for fine res data  ###      ###       ###
#g1_raster <- rasterize(x=g1, y=b, field=g1@data[,1], background=NA, fun="max")
inNP_raster <- rasterize(x=inNP, y=b, field=1, background=NA, fun="first")
plot(inNP_raster)
outNP_raster <- rasterize(x=outNP, y=b, field=1, background=NA, fun="first")
plot(outNP_raster, add=T)

plot(inNP, add=T); plot(outNP, add=T)



#convert and combine --
tmp1 <- as.data.frame( inNP_raster, xy=TRUE)
tmp2 <- as.data.frame( outNP_raster, xy=TRUE)

tmp3 <- as.data.frame( ders$depth, xy = TRUE)
tmp4 <- as.data.frame( ders$slope, xy = TRUE)
tmp5 <- as.data.frame( ders$tpi, xy = TRUE)

# Join data for NPZ6 and adjacent analysis --

# Griffen data --
Dat <- cbind( tmp1, tmp2[,3])
Dat <- cbind( Dat, tmp3[,3])
Dat <- cbind( Dat, tmp4[,3])
Dat <- cbind( Dat, tmp5[,3])

head(Dat)


# Set column names --
df.names <- c("Eastern", "Northing", "inNP", "outNP", "depth", "slope", "tpi")
names(Dat) <- df.names
head(Dat)


# Save raster dfs rds ----
saveRDS(Dat, file= paste0(paste(d.dir, paste("Data" , study, sep='_'), sep='/'), ".RDS"))




