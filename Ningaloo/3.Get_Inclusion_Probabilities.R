### Create inclusion probabilities ####

## FROM Jac's script ----

library( MBHdesign)
library( sp)
library( raster)
library( rgdal)
library( fields)
library( rasterVis)
library( tidyverse)


# clear environment ----
rm(list = ls())


# Set names ----
study <- "Ningaloo_Parks"

platform <- "BOSS"

design.version <- "v1"


# Directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "~/MBH_AbroNPZs"
o.dir <- paste(w.dir, "outputs", sep = '/')
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "shapefiles", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')



# Read in data ----

dat <- readRDS(paste(d.dir,"Data_Ningaloo_Parks.RDS", sep ='/'))
#rast <- readRDS(paste(d.dir, "rasters_Ningaloo_Parks.RDS", sep='/'))
rast <- stack(paste(r.dir, "Ningaloo_predictors.tif", sep='/'))

names(rast) <- c("depth", "slope", "tpi")

# separate covariates ----
d <- rast$depth
s <- rast$slope


plot(d)
plot(s)


hist(d)
hist(s)


# convert depth into a data.frame for ease
d[is.na(d$depth)] <- 0 #convert all the NA to 0 as we don't want to sample this area
d.mat <- as.matrix( d)
d <- as.data.frame(d, xy = T)
d <- as.data.frame(
  cbind( coordinates( d), as.numeric( d.mat)))
colnames( d) <- c("Easting", "Northing", "Depth")
d <- d[order(d$Northing,
                    d$Easting),]

head(d)
length(d$Easting)

# convert slope into a data.frame for ease
s[is.na(s$slope)] <- 0 #convert all the NA to 0 as we don't want to sample this area
s.mat <- as.matrix( s)
s <- as.data.frame(s, xy = T)
s <- as.data.frame(
  cbind( coordinates( s), as.numeric( s.mat)))
colnames( s) <- c("Easting", "Northing", "Slope")
s <- s[order(s$Northing,
                   s$Easting),]
head(s)

#Let's combine bath and slope into the one dataframe
m_combined<- d%>%
  full_join(s, by = c("Easting" = "Easting", "Northing" = "Northing"))%>%
  glimpse()


# remove NAs--
#m_combined[is.na( m_combined)] <- -9999

min(m_combined$Depth)
cellStats(rast$depth, 'min')
cellStats(rast$depth, 'max')
min(m_combined$Slope)
cellStats(rast$slope, 'min')
cellStats(rast$slope, 'max')


## check what cuts ----
s.quant <- c(0,0.4, 0.9, 1)
s.cuts <- quantile(rast$slope, s.quant)
s.cuts # -200 -149 -118  -86 

# get cuts and combine classes ----
m_combined<- m_combined%>%
  mutate(bathyClass   = cut( m_combined$Depth, breaks=c(-400,-350,-300,-250,-200), 
                             labels=c("Deepest","Deep","Mid","Shallowest")))%>%
  mutate(slopeClass   = cut( m_combined$Slope, breaks = s.cuts,
                             #breaks=c(2.775558e-17, 2.743364e-02, 5.469866e-02, 1.128299e-01),
                           labels=c("Flat","ModSlope","Steep")))%>%
  mutate(combinedClass = as.factor(with(., interaction(bathyClass,slopeClass))))%>%
  glimpse()

levels(m_combined$combinedClass)


# # remove NAs
# length(m_combined$Easting) # 112897
# m_combined <- m_combined[!is.na(m_combined$combinedClass), ]
# length(m_combined$Easting) # 9633

#Check we don't have any NAs
summary( m_combined$bathyClass)
summary( m_combined$slopeClass)
summary( m_combined$combinedClass)


#number of drops----
n <- 200 #CHANGE BACK TO 300
# #force the breaks so R doesn't use pretty
tabl <- table( m_combined$combinedClass)

#define the inclusion probs for each class
ncells <- sum( !is.na( m_combined$Depth))

combined.levels <- data.frame( combinedClass=c("Deepest.Flat",
                                               "Deep.Flat",
                                               "Mid.Flat",
                                               "Shallowest.Flat",
                                               "Deepest.ModSlope",
                                               "Deep.ModSlope",
                                               "Mid.ModSlope",
                                               "Shallowest.ModSlope",
                                               "Deepest.Steep",
                                               "Deep.Steep",
                                               "Mid.Steep",
                                               "Shallowest.Steep"), 
                               rel.import=c(0.075,
                                            0.075,
                                            0.075,
                                            0.075,
                                            0.075,
                                            0.075,
                                            0.075,
                                            0.075,
                                            0.1,
                                            0.1,
                                            0.1,
                                            0.1))
                                          


combined.levels$SampProb <- tabl / ncells
head(combined.levels)
combined.levels$SampProb<- as.numeric(combined.levels$SampProb)
combined.levels$incl.probs <- combined.levels$rel.import / combined.levels$SampProb
combined.levels <- replace(combined.levels, is.na(combined.levels), 0)
combined.levels$incl.probs <- combined.levels$incl.probs / sum( combined.levels$incl.probs)

#Define class a character for joining between 
combined.levels$combinedClass <- as.character( combined.levels$combinedClass)
m_combined$combinedClass <- as.character( m_combined$combinedClass)

unique(combined.levels$combinedClass)
unique(m_combined$combinedClass)
length(m_combined$Easting)



#Create design by joining combined leve
design <- m_combined%>%
  inner_join(combined.levels, by = "combinedClass")

min(design$incl.probs )
max(design$incl.probs )
length(design$Easting)


#these next two lines shouldn't be necessary.  But I'll rescale in any case (just for my own checking)
design$inclProb <- design$incl.probs / sum( design$incl.probs, na.rm=TRUE)
sum(design$inclProb)
any(is.na(design))
class(design)
design$inclProb <- design$incl.probs * n 


#checking
print( tapply( design$inclProb, design$combinedClass, sum, na.rm=TRUE))

#convert incl. probs into a raster
inclProb_raster <- rasterFromXYZ( xyz=design[,c("Easting","Northing","incl.probs")])

#inclProb25_raster <- aggregate( inclProb_raster, fact=5, fun= max)# only need to run in massive datasets

plot(inclProb_raster)

cellStats(inclProb_raster, 'sum')

# other way to plot --
# coordinates(design) <- ~Easting+Northing
# gridded(design) <- TRUE
# altIncProbs <- raster(design)
# plot(altIncProbs)
# cellStats(altIncProbs, 'sum')


writeRaster( inclProb_raster, file=paste0(paste(d.dir, paste("inclProbs" , study, platform, design.version, sep='-'), sep='/'), ".tif"), overwrite = TRUE)



