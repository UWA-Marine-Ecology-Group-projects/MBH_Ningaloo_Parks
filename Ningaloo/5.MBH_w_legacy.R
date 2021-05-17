###   ###   ###   MBH design clustered BRUVs    ###   ###   ###


# libraries ----
#install.packages("MBHdesign")
library( MBHdesign)
library( parallel)
library( class)
library( fields)
#install.packages("pdist")
library( pdist)
library( raster)
library( rgdal)
library( sp)
library( rgeos)


# clear environment ----
rm(list = ls())

# Directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "~/MBH_AbroNPZs"
p.dir <- paste(w.dir, "plots", sep = '/')
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "shapefiles", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')
o.dir <- paste(w.dir, "outputs", sep='/')

####    NPZ 6   ####

# Read in the inclusion probs ----
# cov.cuts <- raster(paste(d.dir, "Tpi_cuts-PtCloates-MBH-BOSS-v1.tif", sep='/'))
# plot(cov.cuts)
inclProbs <- raster(paste(d.dir, "inclProbs-Ningaloo_Parks-BOSS-v1.tif", sep='/'))
plot(inclProbs)
inclProbs <- setValues( inclProbs, values( inclProbs) / sum( values( inclProbs), na.rm=TRUE))
plot(inclProbs)

# check sun of incl probs --
cellStats(inclProbs, 'sum')

rootInclProbs <- inclProbs


#rootInclProbs <- setValues( rootInclProbs, sqrt( values( rootInclProbs)))
rootInclProbs <- setValues( rootInclProbs, values( rootInclProbs)*200)
cellStats(rootInclProbs, 'sum')

cellStats(rootInclProbs, 'sum')
plot(rootInclProbs)


# Read data ----
rast <- readRDS(paste(d.dir, "rasters_Ningaloo_Parks.RDS", sep='/'))
#if( class( BRUVS) != "SpatialPointsDataFrame")
#Deans <- SpatialPointsDataFrame( coords=Deans[,c("Longitude","Latitude")], data=Deans, proj4string = CRS( proj4string( zones[[1]])))
#proj4string(Deans) <- proj4string(swrast$bathy)


### MBH without legacy ----

ip <- as.data.frame(inclProbs, xy=T)
head(ip)

samp<- quasiSamp( n=200, dimension=2,
                  potential.sites = ip[,c("x","y")],
                  inclusion.probs=ip$inclProbs.Ningaloo_Parks.BOSS.v1,
                  nSampsToConsider=20000)

plot(inclProbs)
points( samp[,c("x","y")], pch=20, cex=0.3, col = "black") 


## Check how far apart they are ---
## Get CRS in utm ----
crs1 <- CRS("+init=epsg:32750") # WGS 84 / UTM zone 50S


## transform the points into UTM --
p1u <- spTransform(allsites, crs1)

## calculate if 2 points fall within 400 m of eachother ----
# https://gis.stackexchange.com/questions/102796/remove-points-within-x-distance

dist1 <- gDistance(p1u, byid =T)
dist1
max(dist1)
min(dist1[dist1 > 0]) # minimum distance other than 0

## p1 ----
p1_matrix <- gWithinDistance(p1u, dist = 50, byid = TRUE)
diag(p1_matrix) <- NA
p1_matrix

# extract the upper triangular part of matrix and use the column sums as a criterion to remove the points:

p1_matrix[lower.tri(p1_matrix, diag=TRUE)] <- NA
p1_matrix

colSums(p1_matrix, na.rm=TRUE) == 0
v1 <- colSums(p1_matrix, na.rm=TRUE) == 0
p1u[v1, ] # 24 features left

remaining.sites <- p1u[v1, ]
remaining.sites <- spTransform(remaining.sites, proj4string(legacy))
head(remaining.sites)

# plot --
plot(inclProbs, main = "Inclusion probabilities")
plot(cov.cuts, main = "TPI cuts")
plot(rast$tpi, main = "TPI")
plot(zones$All, add=T)
remaining.sites$type  <- as.factor(remaining.sites$type)
remaining.sites$zone  <- as.factor(remaining.sites$zone)
plot(remaining.sites, pch=20, add=T)
plot(remaining.sites, col=remaining.sites$type, pch = 20, add=T) # 41
# rename zone
levels(remaining.sites$zone)[levels(remaining.sites$zone)=="inN"] <- "inNP"
levels(remaining.sites$zone)[levels(remaining.sites$zone)=="out"] <- "outNP"
levels(remaining.sites$zone)
head(remaining.sites)



## Save --
site <- "Ningaloo_Parks"
design <- "200BOSS"
version <- "v1"

writeOGR(samp, o.dir, paste(site, design, version, sep='-'), driver = "ESRI Shapefile", overwrite = T)




############################
####  Spatial sample of new sites ----
####  from altered incl. probs.
############################

### Here use quasiSamp to get random points ####
## these points will be the center of buffer for transects ###

####  Set the seed for reproducability
#set.seed( 777)
#### HAVE NOT BEEN ABLE TO MAKE THIS FUNCTION WORK ----

# no. samples

newSites <- list(inNP = NULL, outNP = NULL)

for( zz in c("inNP", "outNP")){
  print( zz)
  #the number of samples to take (specified minus the legacy number)
  #numby <- floor( (straw.nums[zz])/4)  # for clustered cluster - without legacy sites
  numby <- floor( (straw.nums[zz])) # for not clustered sites
  #numby <- floor( (straw.nums[zz] - numRef[zz])/2)
  #numby <- floor( (straw.nums[zz] - numRef[zz])) # with legacy sites 
  #set up spatial domain
  myZone <- zones[[zz]]
  #if( zz == "AMP"){
  # myZone = zones$AMP - zones$IUCN2
  #set.seed( 747)
  #}
  #tmpIP <- mask( rootInclProbs, myZone)
  tmpIP <- mask( inclProbs, myZone)
  tmpIP <- crop( tmpIP, myZone)
  #take the sample of clusters based on root incl probs
  newSites[[zz]] <- quasiSamp( n=numby, potential.sites=coordinates( tmpIP), inclusion.probs=values(tmpIP), nSampsToConsider=5000)
  
  #plotting (maybe remove at a later date?)
  tmpIPFull <- mask( inclProbs, myZone)
  tmpIPFull <- crop( tmpIPFull, myZone)
  plot( tmpIPFull)
  #plot( legacySites, add=TRUE, pch=1, col='red')
  points( newSites[[zz]][,c("x","y")], pch=20, col='black')
}
newSites <- do.call( "rbind", newSites)
head(newSites)

# Give id to sites and zones --
site.names <- row.names(newSites)
newSites$site <- as.factor(site.names)
#zone.names <- gsub('.{3}$', '', site.names) # remove last 3 characters
zone.names <- substr(site.names, 1, 3) # extract first three characters
newSites$zone <- as.factor(zone.names)
newSites$zone
head(newSites)
newSites$type <- "MBH"
newSites <- SpatialPointsDataFrame( coords=newSites[,c("x","y")], data=newSites, proj4string=CRS(proj4string(inclProbs)))
#some of the spatial balance is not great...  Presumably because the balance of the reference sites is also not great...

# read legacy sites to plot ----
legacy <- readOGR(paste(s.dir, "legacy_Point_Cloates.shp", sep='/'))
head(legacy)



# Plot --
plot(inclProbs)
plot(rast$tpi, main = "TPI")
plot(cov.cuts)
plot(zones$All, add=T)
plot(newSites, col=newSites$zone, pch = 20, add=T) # 41
plot(legacy, col = "blue", pch=20, add=TRUE)
newSites$zone


# join sites and legacy ----
proj4string(legacy) <- proj4string(rast$depth)
proj4string(newSites) <- proj4string(rast$depth)
allsites <- raster::union(newSites, legacy)
head(allsites)

ID <- paste(1:50)
allsites$uniqueID <- ID
head(allsites)


### Make sure the clusters centres are ~ 1 km apart ----


## Get CRS in utm ----
crs1 <- CRS("+init=epsg:32750") # WGS 84 / UTM zone 50S


## transform the points into UTM --
p1u <- spTransform(allsites, crs1)

## calculate if 2 points fall within 400 m of eachother ----
# https://gis.stackexchange.com/questions/102796/remove-points-within-x-distance

dist1 <- gDistance(p1u, byid =T)
dist1
max(dist1)
min(dist1[dist1 > 0]) # minimum distance other than 0

## p1 ----
p1_matrix <- gWithinDistance(p1u, dist = 50, byid = TRUE)
diag(p1_matrix) <- NA
p1_matrix

# extract the upper triangular part of matrix and use the column sums as a criterion to remove the points:

p1_matrix[lower.tri(p1_matrix, diag=TRUE)] <- NA
p1_matrix

colSums(p1_matrix, na.rm=TRUE) == 0
v1 <- colSums(p1_matrix, na.rm=TRUE) == 0
p1u[v1, ] # 24 features left

remaining.sites <- p1u[v1, ]
remaining.sites <- spTransform(remaining.sites, proj4string(legacy))
head(remaining.sites)

# plot --
plot(inclProbs, main = "Inclusion probabilities")
plot(cov.cuts, main = "TPI cuts")
plot(rast$tpi, main = "TPI")
plot(zones$All, add=T)
remaining.sites$type  <- as.factor(remaining.sites$type)
remaining.sites$zone  <- as.factor(remaining.sites$zone)
plot(remaining.sites, pch=20, add=T)
plot(remaining.sites, col=remaining.sites$type, pch = 20, add=T) # 41
# rename zone
levels(remaining.sites$zone)[levels(remaining.sites$zone)=="inN"] <- "inNP"
levels(remaining.sites$zone)[levels(remaining.sites$zone)=="out"] <- "outNP"
levels(remaining.sites$zone)
head(remaining.sites)

## Save --
site <- "Pt-Cloates"
design <- "50BOSS"
version <- "v2"

writeOGR(remaining.sites, o.dir, paste(site, design, version, sep='-'), driver = "ESRI Shapefile", overwrite = T)



