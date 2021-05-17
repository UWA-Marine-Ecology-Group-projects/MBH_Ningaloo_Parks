#### Alter inclusion probabilities for legacy sites ----


# libraries ----
library( rgdal)
library( sp)
library( raster)
library( MBHdesign)
library(dplyr)


# clear environment ----
rm(list = ls())

# Directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "~/MBH_AbroNPZs"
o.dir <- paste(w.dir, "outputs", sep = '/')
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "shapefiles", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')

# Information for files ---

study <- "Ningaloo_Parks"

platform <- "BOSS"

design.version <- "v2"

total.no.deployments <- "200deployments"



# Read legacy sites ----
legacy <- readOGR(paste(s.dir, "remaining_legacy.shp", sep='/'))
proj4string(legacy)
plot(legacy, pch=20)

legacys <- as.data.frame(legacy)
legacyss <- legacys[,c(5,6)]
legacyss
#legacyss <- legacyss[c(1,8),]
# in ascending longitude
# legacyss <- arrange(legacyss, coords.x1)
# names(legacyss) <- c("x", "y")
# class(legacyss)

# make matrix
#legacyss <- legacyss[c(1:5),]
#class(legacyss)
lega <- as.matrix(legacyss)
class(lega)
lega


# Read inclusion probabilities ----
inclProbs <- raster(paste(d.dir, "inclProbs-Ningaloo_Parks-BOSS-v1.tif", sep='/'))
plot(inclProbs)
plot(legacy, pch = 20, add=T)
cellStats(inclProbs, 'sum')
inclProbs <- inclProbs/350.135
cellStats(inclProbs, 'sum')


# make sum of inc probs the number of samples needed ---
rootInclProbs <- inclProbs
#rootInclProbs <- setValues( rootInclProbs, sqrt( values( rootInclProbs)))
rootInclProbs <- setValues( rootInclProbs, values( rootInclProbs)*200)
cellStats(rootInclProbs, 'sum')
plot(rootInclProbs)

inclProbs <- rootInclProbs
plot(inclProbs)
cellStats(inclProbs, 'sum')


### Split incl probs ----
e <- drawExtent()
inclProbs <- crop(inclProbs, e)
plot(inclProbs)
plot(legacy, pch = 20, add=T)

legacy1 <- raster::extract(inclProbs, legacy, sp = T)
legacy1
# remove rows w na
legacy1 <- legacy1[!is.na(legacy1$inclProbs.Ningaloo_Parks.BOSS.v1), ]
legacy1 # 5 points left

# make a matrix of legacy ----
legacys <- as.data.frame(legacy1)
legacyss <- legacys[,c(5,6)]
legacyss
lega <- as.matrix(legacyss)
class(lega)
lega

# test dissagregating inclusion probs ----
inclProbs <- raster::disaggregate(inclProbs, 10)
plot(inclProbs)
cellStats(inclProbs, 'sum')

# potential sites ----
pot.sites <- as.data.frame(inclProbs, xy = TRUE)
head(pot.sites)
pot.sites <- arrange(pot.sites, y)
head(pot.sites)
class(pot.sites)

# as df
pot.sitess <- pot.sites[,c(1,2)]
head(pot.sitess)
potsmat <- as.matrix(pot.sitess)
class(potsmat)

# inclusion probs as numeric or df
#ip <- as.data.frame(pot.sites[,3])
ip <- pot.sites[,3]
class(ip)
ip
length(ip)


# alter inclProbs test1----
altInclProbs <- alterInclProbs(legacy.sites = lega, 
                               potential.sites = potsmat,
                               #n = 200,
                               inclusion.probs = ip)
#mc.cores = 6)
class(lega)
class(potsmat)
class(ip)

inclProbs


# plot(altInclProbs)
# class(altInclProbs)

#visualise
image( x=unique( potsmat[,1]), y=unique( potsmat[,2]),
        z=matrix( ip, nrow=100, ncol=43),
        main="Inclusion Probabilities (Undadjusted)",
        ylab=colnames( potsmat)[2], xlab=colnames( potsmat)[1])

image( x=unique( potsmat[,1]), y=unique( potsmat[,2]),
       #z=matrix( altInclProbs, nrow=3405, ncol=7215),
       z=matrix( altInclProbs, nrow=167, ncol=294),
       main="Adjusted Inclusion Probabilities",
       ylab=colnames( potsmat)[2], xlab=colnames( potsmat)[1])


# Altered inclusion probabilities as raster ----

aIP <- cbind(pot.sitess, altInclProbs)
head(aIP)
class(aIP)

coordinates(aIP) <- ~x+y
gridded(aIP) <- TRUE
altIncProbs <- raster(aIP)
plot(altIncProbs)
cellStats(altIncProbs, 'sum')
# aggregate raster
altip <- aggregate(altIncProbs, 10)
cellStats(altip, 'sum')
plot(altip)

writeRaster( altip, file=paste0(paste(d.dir, paste("altIncProbs" , study, platform, design.version, sep='-'), sep='/'), ".tif"), overwrite = TRUE)
