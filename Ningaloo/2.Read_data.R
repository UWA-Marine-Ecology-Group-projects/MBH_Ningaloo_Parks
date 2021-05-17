###   ###   ###   Read data for MBH analysis    ###   ###   ###



# libraries ----
library( rgdal)
library( sp)
library( raster)


# clear environment ----
rm(list = ls())



# Set names ----
study <- "Ningaloo_Parks"



# Directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "~/MBH_AbroNPZs"
o.dir <- paste(w.dir, "outputs", sep = '/')
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "shapefiles", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')




## Read raster data ----
ders <- stack(paste(r.dir, "Ningaloo_predictors.tif", sep ='/'))
plot(ders)
names(ders) <-  c("depth","slope", "tpi")
plot(ders)


# Save rasters rds ----
rasterfiles <- list()
rasterfiles$depth <- ders$depth
rasterfiles$slope <- ders$slope
rasterfiles$tpi <- ders$tpi

# Save raster data ----
saveRDS(rasterfiles, file= paste0(paste(d.dir, paste("rasters" , study, sep='_'), sep='/'), ".RDS"))



# Save data as df --

tmp1 <- as.data.frame( ders$depth, xy = TRUE)
tmp2 <- as.data.frame( ders$slope, xy = TRUE)
tmp3 <- as.data.frame( ders$tpi, xy = TRUE)

# Join data for NPZ6 and adjacent analysis --

# Griffen data --
Dat <- cbind( tmp1, tmp2[,3])
Dat <- cbind( Dat, tmp3[,3])

head(Dat)


# Set column names --
df.names <- c("Eastern", "Northing", "depth", "slope", "tpi")
names(Dat) <- df.names
head(Dat)


# Save raster dfs rds ----
saveRDS(Dat, file= paste0(paste(d.dir, paste("Data" , study, sep='_'), sep='/'), ".RDS"))


# read Legacy sites ----

lega <- readOGR(paste(s.dir, "Points_from_Steve.shp", sep='/'))
lega # 24 points

plot(ders$depth)
plot(lega, pch=20, add=T)

# which legacy sites within domain --
lega2 <- raster::extract(ders$depth, lega, sp = T)
lega2

lega2 <- lega2[!is.na(lega2$depth),]
lega2 # 13 points left
proj4string(lega2)

# save remaining legacy points ----
writeOGR(lega2, s.dir, "remaining_legacy", driver = "ESRI Shapefile")
