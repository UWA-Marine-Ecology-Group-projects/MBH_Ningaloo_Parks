##Elizabeth and Middleton reefs BRUV sampling design
##Prepared by Jac Monk under the amazing guidance of the one and only Scott Foster
##21/11/2019

# Clear memory
rm(list=ls())

#getwd()
setwd("C:\\Users\\jmonk1\\Dropbox\\NESP\\EnM\\Elizabeth-Middleton Survey Planning\\BRUV plan")

library( MBHdesign)
library( sp)
library( raster)
library( rgdal)
library( fields)
library( rasterVis)
library( tidyverse)

set.seed( 66)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########################################MIDDLETON##############################
#load ascii Load each region seperately
m_bth <- read.asciigrid("m_bth_cl150.asc",
                           proj4string = CRS("+proj=utm +zone=57 +datum=WGS84"))
plot(m_bth)

m_tpi <- read.asciigrid("m_tpi_cl150.asc",
                        proj4string = CRS("+proj=utm +zone=57 +datum=WGS84"))
plot(m_tpi)

#convert bath into a data.frame for ease
bth.mat <- as.matrix( m_bth)
m_bth <- as.data.frame(
  cbind( coordinates( m_bth), as.numeric( bth.mat)))
colnames( m_bth) <- c("Easting", "Northing", "Depth")
m_bth <- m_bth[order( m_bth$Northing,
                            m_bth$Easting),]

#m_bth[m_bth == 'NA'] <- 0 #convert all the NA to 0 as we don't want to sample this area
#m_bth<- na.omit(m_bth)

#convert tpi into a data.frame for ease
tpi.mat <- as.matrix( m_tpi)
m_tpi <- as.data.frame(
  cbind( coordinates( m_tpi), as.numeric( tpi.mat)))
colnames( m_tpi) <- c("Easting", "Northing", "TPI")
m_tpi <- m_tpi[order( m_tpi$Northing,
                      m_tpi$Easting),]
#m_tpi[m_tpi == 'NA'] <- 0 #convert all the NA to 0 as we don't want to sample this area

#Let's combine bath and tpi into the one dataframe
m_combined<- m_bth%>%
            full_join(m_tpi, by = c("Easting" = "Easting", "Northing" = "Northing"))%>%
             glimpse()
m_combined[is.na( m_combined)] <- -9999

min(m_combined$Depth)
min(m_combined$TPI)

m_combined<- m_combined%>%
            mutate(bathyClass   = cut( m_combined$Depth, breaks=c(-10000,-695,-150,-100,-50,-20,-10, 4), 
                                      labels=c("Outside","Deep","Deep","Mid_Meso","Upper_Meso","Transition", "Shallow")))%>%
            mutate(tpiClass   = cut( m_combined$TPI, breaks=c(-10000, -389, 0, 0.1, 0.4, 170),
                                       labels=c("Outside","Depression_Flat","Depression_Flat","ModTPI","HighTPI")))%>%
            mutate(combinedClass = as.factor(with(., interaction(bathyClass,tpiClass))))%>%
              glimpse()

#Check we don't have any NAs
summary( m_combined$bathyClass)
summary( m_combined$tpiClass)
summary( m_combined$combinedClass)

#number of drops----
n <- 300 #CHANGE BACK TO 300
# #force the breaks so R doesn't use pretty
tabl <- table( m_combined$combinedClass)
# write.csv(tabl,"m.strata.csv")

#define the inclusion probs for each class
ncells <- sum( !is.na( m_combined$Depth))
combined.levels <- data.frame( combinedClass=c("Outside.Outside",
                                               "Deep.Outside",
                                               "Mid_Meso.Outside",
                                               "Upper_Meso.Outside",
                                               "Transition.Outside",
                                               "Shallow.Outside",
                                               "Outside.Depression_Flat",
                                               "Deep.Depression_Flat",
                                               "Mid_Meso.Depression_Flat",
                                               "Upper_Meso.Depression_Flat",
                                               "Transition.Depression_Flat",
                                               "Shallow.Depression_Flat",
                                               "Outside.ModTPI",
                                               "Deep.ModTPI",
                                               "Mid_Meso.ModTPI",
                                               "Upper_Meso.ModTPI",
                                               "Transition.ModTPI",
                                               "Shallow.ModTPI",
                                               "Outside.HighTPI",
                                               "Deep.HighTPI",
                                               "Mid_Meso.HighTPI",
                                               "Upper_Meso.HighTPI",
                                               "Transition.HighTPI",
                                               "Shallow.HighTPI"), 
                               rel.import=c(0,
                                            0,
                                            0,
                                            0,
                                            0,
                                            0,
                                            0,
                                            0.01,
                                            0.04,
                                            0.06,
                                            0.06,
                                            0.25,
                                            0,
                                            0.01,
                                            0.02,
                                            0.05,
                                            0.06,
                                            0.1,
                                            0,
                                            0.02,
                                            0.08,
                                            0.08,
                                            0.06,
                                            0.1))

combined.levels$SampProb <- tabl / ncells
#combined.levels$SampProb[1]=0 #set outside data 0
combined.levels$SampProb<- as.numeric(combined.levels$SampProb)
combined.levels$incl.probs <- combined.levels$rel.import / combined.levels$SampProb
combined.levels <- replace(combined.levels, is.na(combined.levels), 0)
combined.levels$incl.probs <- combined.levels$incl.probs / sum( combined.levels$incl.probs)

#Define class a character for joining between 
combined.levels$combinedClass <- as.character( combined.levels$combinedClass)
m_combined$combinedClass <- as.character( m_combined$combinedClass)

unique(combined.levels$combinedClass)
unique(m_combined$combinedClass)


#Create design by joining combined leve
design <- m_combined%>%
  inner_join(combined.levels,by = "combinedClass")

min(design$incl.probs )
max(design$incl.probs )

#design <- na.omit(design)
#these next two lines shouldn't be necessary.  But I'll rescale in any case (just for my own checking)
design$inclProb <- design$incl.probs / sum( design$incl.probs, na.rm=TRUE)
design$inclProb <- design$incl.probs * n 

#checking
print( tapply( design$inclProb, design$combinedClass, sum, na.rm=TRUE))

#convert incl. probs into a raster
inclProb_raster <- rasterFromXYZ( xyz=design[,c("Easting","Northing","incl.probs")])
#inclProb25_raster <- aggregate( inclProb_raster, fact=5, fun= max)# only need to run in massive datasets

plot(inclProb_raster)

#dev.off()
#convert to a data.frame for transectSamp
inclProb_rasterMat <- as.matrix( inclProb_raster)
inclProb_raster <- as.data.frame(
  cbind( coordinates( inclProb_raster), as.numeric( inclProb_rasterMat)))
colnames( inclProb_raster) <- c("Easting", "Northing", "inclProb")
inclProb_raster <- inclProb_raster[order( inclProb_raster$Northing,
                                              inclProb_raster$Easting),]


#Point sampling
#Take sample
names(design)
# samp <- quasiSamp( n=n, dimension=2,
#                    potential.sites = design[,c("Easting","Northing")],
#                    #inclusion.probs=design$incl.probs, 
#                    nSampsToConsider=1000*n)


samp<- quasiSamp( n=n, dimension=2,
                   potential.sites = design[,c("Easting","Northing")],
                   inclusion.probs=design$incl.probs,
                   nSampsToConsider=1000*n)


points( samp[,c("Easting","Northing")], pch=20, cex=2, col = "red") 

write.csv(samp,"middleton_quasi_bruvs_samp.csv")


inclProb_raster2 <- rasterFromXYZ( xyz=design[,c("Easting","Northing","incl.probs")])

writeRaster(inclProb_raster2,"middleton_quasi_BRUV_inclProb_raster.tif", overwrite=T)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########################################MIDDLETON##############################
#load ascii Load each region seperately
e_bth <- read.asciigrid("e_bth_cl150.asc",
                        proj4string = CRS("+proj=utm +zone=57 +datum=WGS84"))
plot(e_bth)

e_tpi <- read.asciigrid("e_tpi_cl150.asc",
                        proj4string = CRS("+proj=utm +zone=57 +datum=WGS84"))
plot(e_tpi)

#convert bath into a data.frame for ease
bth.mat <- as.matrix( e_bth)
e_bth <- as.data.frame(
  cbind( coordinates( e_bth), as.numeric( bth.mat)))
colnames( e_bth) <- c("Easting", "Northing", "Depth")
e_bth <- e_bth[order( e_bth$Northing,
                      e_bth$Easting),]

#m_bth[m_bth == 'NA'] <- 0 #convert all the NA to 0 as we don't want to sample this area
#m_bth<- na.omit(m_bth)

#convert tpi into a data.frame for ease
tpi.mat <- as.matrix( e_tpi)
e_tpi <- as.data.frame(
  cbind( coordinates( e_tpi), as.numeric( tpi.mat)))
colnames( e_tpi) <- c("Easting", "Northing", "TPI")
e_tpi <- e_tpi[order( e_tpi$Northing,
                      e_tpi$Easting),]
#m_tpi[m_tpi == 'NA'] <- 0 #convert all the NA to 0 as we don't want to sample this area

#Let's combine bath and tpi into the one dataframe
e_combined<- e_bth%>%
  full_join(e_tpi, by = c("Easting" = "Easting", "Northing" = "Northing"))%>%
  glimpse()
e_combined[is.na( e_combined)] <- -9999

min(e_combined$Depth)
min(e_combined$TPI)

e_combined<- e_combined%>%
  mutate(bathyClass   = cut( e_combined$Depth, breaks=c(-10000,-2600,-150,-100,-50,-20,-10, 5), 
                             labels=c("Outside","Deep","Deep","Mid_Meso","Upper_Meso","Transition", "Shallow")))%>%
  mutate(tpiClass   = cut( e_combined$TPI, breaks=c(-10000, -2500, 0, 0.1, 0.4,250),
                           labels=c("Outside","Depression_Flat","Depression_Flat","ModTPI","HighTPI")))%>%
  mutate(combinedClass = as.factor(with(., interaction(bathyClass,tpiClass))))%>%
  glimpse()

#Check we don't have any NAs
summary( e_combined$bathyClass)
summary( e_combined$tpiClass)
summary( e_combined$combinedClass)

#number of drops----
n <- 300 #CHANGE BACK TO 300
# #force the breaks so R doesn't use pretty
tabl <- table( e_combined$combinedClass)
# write.csv(tabl,"m.strata.csv")

#define the inclusion probs for each class
ncells <- sum( !is.na( m_combined$Depth))
combined.levels <- data.frame( combinedClass=c("Outside.Outside",
                                               "Deep.Outside",
                                               "Mid_Meso.Outside",
                                               "Upper_Meso.Outside",
                                               "Transition.Outside",
                                               "Shallow.Outside",
                                               "Outside.Depression_Flat",
                                               "Deep.Depression_Flat",
                                               "Mid_Meso.Depression_Flat",
                                               "Upper_Meso.Depression_Flat",
                                               "Transition.Depression_Flat",
                                               "Shallow.Depression_Flat",
                                               "Outside.ModTPI",
                                               "Deep.ModTPI",
                                               "Mid_Meso.ModTPI",
                                               "Upper_Meso.ModTPI",
                                               "Transition.ModTPI",
                                               "Shallow.ModTPI",
                                               "Outside.HighTPI",
                                               "Deep.HighTPI",
                                               "Mid_Meso.HighTPI",
                                               "Upper_Meso.HighTPI",
                                               "Transition.HighTPI",
                                               "Shallow.HighTPI"), 
                               rel.import=c(0,
                                            0,
                                            0,
                                            0,
                                            0,
                                            0,
                                            0,
                                            0.01,
                                            0.04,
                                            0.06,
                                            0.06,
                                            0.25,
                                            0,
                                            0.01,
                                            0.02,
                                            0.05,
                                            0.06,
                                            0.1,
                                            0,
                                            0.02,
                                            0.08,
                                            0.08,
                                            0.06,
                                            0.1))

combined.levels$SampProb <- tabl / ncells
#combined.levels$SampProb[1]=0 #set outside data 0
combined.levels$SampProb<- as.numeric(combined.levels$SampProb)
combined.levels$incl.probs <- combined.levels$rel.import / combined.levels$SampProb
combined.levels <- replace(combined.levels, is.na(combined.levels), 0)
combined.levels$incl.probs <- combined.levels$incl.probs / sum( combined.levels$incl.probs)

#Define class a character for joining between 
combined.levels$combinedClass <- as.character( combined.levels$combinedClass)
e_combined$combinedClass <- as.character( e_combined$combinedClass)

unique(combined.levels$combinedClass)
unique(e_combined$combinedClass)


#Create design by joining combined leve
design <- e_combined%>%
  inner_join(combined.levels,by = "combinedClass")

min(design$incl.probs )
max(design$incl.probs )

#design <- na.omit(design)
#these next two lines shouldn't be necessary.  But I'll rescale in any case (just for my own checking)
design$inclProb <- design$incl.probs / sum( design$incl.probs, na.rm=TRUE)
design$inclProb <- design$incl.probs * n 

#checking
print( tapply( design$inclProb, design$combinedClass, sum, na.rm=TRUE))

#convert incl. probs into a raster
inclProb_raster <- rasterFromXYZ( xyz=design[,c("Easting","Northing","incl.probs")])
#inclProb25_raster <- aggregate( inclProb_raster, fact=5, fun= max)# only need to run in massive datasets

plot(inclProb_raster)

#dev.off()
#convert to a data.frame for transectSamp
inclProb_rasterMat <- as.matrix( inclProb_raster)
inclProb_raster <- as.data.frame(
  cbind( coordinates( inclProb_raster), as.numeric( inclProb_rasterMat)))
colnames( inclProb_raster) <- c("Easting", "Northing", "inclProb")
inclProb_raster <- inclProb_raster[order( inclProb_raster$Northing,
                                          inclProb_raster$Easting),]


#Point sampling
#Take sample
#names(design)


samp<- quasiSamp( n=n, dimension=2,
                  potential.sites = design[,c("Easting","Northing")],
                  inclusion.probs=design$incl.probs,
                  nSampsToConsider=1000*n)


points( samp[,c("Easting","Northing")], pch=20, cex=2, col = "red") 

write.csv(samp,"elizabeth_quasi_bruvs_samp.csv")


inclProb_raster2 <- rasterFromXYZ( xyz=design[,c("Easting","Northing","incl.probs")])

writeRaster(inclProb_raster2,"elizabeth_quasi_BRUV_inclProb_raster.tif", overwrite=T)





#Transect Sampling for later

#S shaped pattern to AUV transects
pattern <- matrix( c(0,0, 0.05,0, 0.05,0.05, 0,0.05, 0,0.1, 0.05,0.1), ncol=2, byrow=TRUE)
m <- 4
pattern1 <- matrix( NA, ncol=2, nrow=(nrow( pattern)-1)*(m))
for( ii in 1:(nrow( pattern)-1)){
  pattern1[(ii-1)*m+1:m,1] <- seq( from=pattern[ii,1], to=pattern[ii+1,1], length=m)
  pattern1[(ii-1)*m+1:m,2] <- seq( from=pattern[ii,2], to=pattern[ii+1,2], length=m)
}
pattern1 <- pattern1[!duplicated( pattern1),]

plot(pattern1)

#Set up transect parameters----
control <- list(transect.pattern='line', 
                transect.nPts=25, #good idea to have one in each cell
                line.length=200, # length of transect in m
                nRotate=19, #less can be quicker but reduces possible directions
                edgeAdjust=TRUE, 
                gamma=0.25, 
                edge.max.iter=25, 
                nSampsToConsider=50000)


#Take sample
middle.t <- transectSamp( n=n, study.area=NULL, 
                            potential.sites=inclProb_raster[,c("Easting", "Northing")],
                            inclusion.probs=inclProb_raster$inclProb,
                            #constrainedSet=descendConstraint,
                            control=control)


#Plot the design
with(design, image.plot( uniqueEast, uniqueNorth, LocationMat,
                         xlab="", ylab="", main="Spatially-Balanced Sample", asp=1,
                         ylim=NLims, xlim=ELims,
                         col=rev(tim.colors())))
lines( middle.t[,c("transect.Easting","transect.Northing")],col='black', lwd=1.5)

write.csv(middle.t,"hunter_amp_tv_e_samp.csv")

