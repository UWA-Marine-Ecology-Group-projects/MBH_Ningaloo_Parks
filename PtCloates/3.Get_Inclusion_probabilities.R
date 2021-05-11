### Create inclusion probabilities ####

library( rgdal)
library( sp)
library( raster)


# clear environment ----
rm(list = ls())


# Set names ----
study <- "PtCloates-MBH"

platform <- "Bruvs"

design.version <- "v1"


# Directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#w.dir <- "~/MBH_AbroNPZs"
o.dir <- paste(w.dir, "outputs", sep = '/')
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "shapefiles", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')



# Read in data ----

dat <- readRDS(paste(d.dir,"Data_PtCloates.RDS", sep ='/'))
rast <- readRDS(paste(d.dir, "rasters_PtCloates.RDS", sep='/'))
zones <- readRDS(paste(d.dir, "Zones_PtCloates.RDS", sep='/'))


# straw number for each zone

straw.nums <- c(12, 12)  # for BRUVs - numbers of drops rest w structure + caut with structure,cau wout str, open w str, open wout str
#straw.nums <- c(6,6,6,6,6,6,8,6,6,20,28,12,28) # for BOSS
straw.props <- straw.nums / sum( straw.nums)
straw.props
names( straw.nums) <- names( straw.props) <- c("inNP", "outNP")
saveRDS(straw.nums, file = paste0(paste(d.dir, paste("StrawmanNumbers" , study, platform, design.version, sep='_'), sep='/'), ".RDS"))


# Get bathy cut points ----
# And their numbers of drops

#Bathy.quant <- c(0,0.20, 0.66, 1)
hist(rast$depth)
Bathy.quant <- c(0,0.3, 0.7, 1)
Bathy.cuts <- quantile(rast$depth, Bathy.quant)#c( -Inf,0.02,0.04,0.08,0.16,Inf)
Bathy.cuts # -200 -149 -118  -86 
#trying to make it so there is no hand-picking (except for the hand-picked function)
tmp <- cumsum( Bathy.quant)
tmp
#Bathy.targetNums <- rep( floor( 18/8), 4)#floor( ( tmp / sum( tmp))[-1] * 200)#rep( 40, 5)#c( 20,20,30,65,65)
Bathy.targetNums <- rep(floor( ( tmp / sum( tmp))[-1] * sum(straw.nums)))
Bathy.targetProps <-  Bathy.targetNums / sum( Bathy.targetNums)
Bathy.targetProps

# might need to alter these proportions by hand, so that the middle interval is better represented, as all structures fall within that interval


#### Proportion of potential sites in each zone

Dat_small <- dat[!is.na( dat$depth),]
tmp <- colSums( Dat_small[,c("inNP", "outNP")], na.rm=TRUE) 
#tmp[2] <- tmp[2] - tmp[1] # so similar amount of sites in SPZ and MUZ
props <- tmp / nrow( Dat_small)
props <- props / sum( props) # 1 UP TO HERE


#### Get Bathy cut points ----

catB <- cut( rast$depth, breaks=Bathy.cuts, na.rm=TRUE)

plot( catB); plot( zones$All, border = "black", add=TRUE) 

writeRaster( catB, file=paste0(paste(d.dir, paste("Bathy_cuts" , study, platform, design.version, sep='-'), sep='/'), ".tif"), overwrite = TRUE)




## Get slope cut points ----
# And their numbers of drops

#Bathy.quant <- c(0,0.20, 0.66, 1)
hist(rast$slope)
slope.quant <- c(0,0.3, 0.7, 1)
slope.cuts <- quantile(rast$slope, slope.quant)#c( -Inf,0.02,0.04,0.08,0.16,Inf)
slope.cuts # -200 -149 -118  -86 
#trying to make it so there is no hand-picking (except for the hand-picked function)
tmp <- cumsum( slope.quant)
tmp
#Bathy.targetNums <- rep( floor( 18/8), 4)#floor( ( tmp / sum( tmp))[-1] * 200)#rep( 40, 5)#c( 20,20,30,65,65)
slope.targetNums <- rep(floor( ( tmp / sum( tmp))[-1] * sum(straw.nums)))
slope.targetProps <-  slope.targetNums / sum( slope.targetNums)
slope.targetProps

# might need to alter these proportions by hand, so that the middle interval is better represented, as all structures fall within that interval


#### Proportion of potential sites in each zone

Dat_small <- dat[!is.na( dat$slope),]
tmp <- colSums( Dat_small[,c("inNP", "outNP")], na.rm=TRUE) 
#tmp[2] <- tmp[2] - tmp[1] # so similar amount of sites in SPZ and MUZ
props <- tmp / nrow( Dat_small)
props <- props / sum( props) # 1 UP TO HERE


#### Get Bathy cut points ----

catS <- cut( rast$slope, breaks=tpi.cuts, na.rm=TRUE)

plot( catS); plot( zones$All, border = "black", add=TRUE) 

writeRaster( catS, file=paste0(paste(d.dir, paste("slope_cuts" , study, platform, design.version, sep='-'), sep='/'), ".tif"), overwrite = TRUE)




## Get TPI cut points ----
# And their numbers of drops

#Bathy.quant <- c(0,0.20, 0.66, 1)
hist(rast$tpi)
tpi.quant <- c(0,0.2, 0.9, 1)
tpi.cuts <- quantile(rast$tpi, tpi.quant)#c( -Inf,0.02,0.04,0.08,0.16,Inf)
tpi.cuts # -200 -149 -118  -86 
#trying to make it so there is no hand-picking (except for the hand-picked function)
tmp <- cumsum( tpi.quant)
tmp
#Bathy.targetNums <- rep( floor( 18/8), 4)#floor( ( tmp / sum( tmp))[-1] * 200)#rep( 40, 5)#c( 20,20,30,65,65)
tpi.targetNums <- rep(floor( ( tmp / sum( tmp))[-1] * sum(straw.nums)))
tpi.targetProps <-  slope.targetNums / sum( tpi.targetNums)
tpi.targetProps

# might need to alter these proportions by hand, so that the middle interval is better represented, as all structures fall within that interval


#### Proportion of potential sites in each zone

Dat_small <- dat[!is.na( dat$tpi),]
tmp <- colSums( Dat_small[,c("inNP", "outNP")], na.rm=TRUE) 
#tmp[2] <- tmp[2] - tmp[1] # so similar amount of sites in SPZ and MUZ
props <- tmp / nrow( Dat_small)
props <- props / sum( props) # 1 UP TO HERE


#### Get Bathy cut points ----

catT <- cut( rast$tpi, breaks=tpi.cuts, na.rm=TRUE)

plot( catT); plot( zones$All, border = "black", add=TRUE) 

writeRaster( catT, file=paste0(paste(d.dir, paste("Tpi_cuts" , study, platform, design.version, sep='-'), sep='/'), ".tif"), overwrite = TRUE)



#### Get inclusion probabiltites ----

# get bathy target props
Bathy.targetProps <- c(0.25,0.25,0.25,0.25) 
#Bathy.targetProps <- c(0.3, 0.4, 0.3) 
#Bathy.targetProps <- c(0.1, 0.4, 0.5) 
#Bathy.targetProps <- Bathy.targetProps
#Bathy.targetProps2 <- c(1)
Bathy.targetProps2 <- c(0, 0.9, 0.1)
Bathy.targetProps3 <- c(0.5,0.5)
Bathy.targetProps4 <- c(1)


# initial raster from strata raster --
inclProbs <- catB
plot(inclProbs)

# Inclusion probs --


for( zz in c( "g1", "g2", "g3", "g4", "g5", "g6", "g7", "g8", "g9", "cs", "cz", "os", "oz")){
  print( zz)
  # if( zz == "os")
  #   zoneID <- extract( x=catB, y=zones$os, cellnumbers=TRUE)
  # zoneID <- extract( x=catB, y=zones$MUZ-zones$NPZ, cellnumbers=TRUE)
  #else
  zoneID <- extract( x=catB, y=zones[[zz]], cellnumbers=TRUE)
  propsOfstrata <- table( catB@data@values[zoneID[[1]][,"cell"]])
  propsOfstrata <- propsOfstrata / sum( propsOfstrata)
  if(length(propsOfstrata) == 4)
    tmp <- Bathy.targetProps / propsOfstrata #the desired inclusion probs (unstandardised)
  else
    if(length(propsOfstrata) == 3)
      tmp <- Bathy.targetProps2 / propsOfstrata #the desired inclusion probs (unstandardised)
  else
    if(length(propsOfstrata) == 2)
      tmp <- Bathy.targetProps3 / propsOfstrata #the desired inclusion probs (unstandardised)
  else 
    tmp <- Bathy.targetProps4 / propsOfstrata #the desired inclusion probs (unstandardised)
  for( ii in 1:length( propsOfstrata)){
    inclProbs[zoneID[[1]][,"cell"]][zoneID[[1]][,"value"]==ii] <- tmp[ii]
  }
  inclProbs[zoneID[[1]][,"cell"]][is.na( inclProbs[zoneID[[1]][,"cell"]])] <- 0
  inclProbs[zoneID[[1]][,"cell"]] <- inclProbs[zoneID[[1]][,"cell"]] / sum( inclProbs[zoneID[[1]][,"cell"]])
}
inclProbs@data@values[inclProbs@data@values %in% c(0,1,2,3,4,5,6,7,8)] <- NA  #cheats way to crop
plot( inclProbs)



#standardising so that the zone totals are correct according to straw.props | straw.nums
g1zone <- extract( x=catB, y=zones$g1, cellnumbers=TRUE)
g2zone <- extract( x=catB, y=zones$g2, cellnumbers=TRUE)
g3Zone <- extract( x=catB, y=zones$g3, cellnumbers=TRUE)
g4Zone <- extract( x=catB, y=zones$g4, cellnumbers=TRUE)
g5Zone <- extract( x=catB, y=zones$g5, cellnumbers=TRUE)
g6Zone <- extract( x=catB, y=zones$g6, cellnumbers=TRUE)
g7Zone <- extract( x=catB, y=zones$g7, cellnumbers=TRUE)
g8Zone <- extract( x=catB, y=zones$g8, cellnumbers=TRUE)
g9Zone <- extract( x=catB, y=zones$g9, cellnumbers=TRUE)
csZone <- extract( x=catB, y=zones$cs, cellnumbers=TRUE)
czZone <- extract( x=catB, y=zones$cz, cellnumbers=TRUE)
osZone <- extract( x=catB, y=zones$os, cellnumbers=TRUE)
ozZone <- extract( x=catB, y=zones$oz, cellnumbers=TRUE)

inclProbs@data@values[g1zone[[1]][,'cell']] <- inclProbs@data@values[g1zone[[1]][,'cell']] * straw.props["g1"]
inclProbs@data@values[g2zone[[1]][,'cell']] <- inclProbs@data@values[g2zone[[1]][,'cell']] * straw.props["g2"]
inclProbs@data@values[g3Zone[[1]][,'cell']] <- inclProbs@data@values[g3Zone[[1]][,'cell']] * straw.props["g3"]
inclProbs@data@values[g4Zone[[1]][,'cell']] <- inclProbs@data@values[g4Zone[[1]][,'cell']] * straw.props["g4"]
inclProbs@data@values[g5Zone[[1]][,'cell']] <- inclProbs@data@values[g5Zone[[1]][,'cell']] * straw.props["g5"]
inclProbs@data@values[g6Zone[[1]][,'cell']] <- inclProbs@data@values[g6Zone[[1]][,'cell']] * straw.props["g6"]
inclProbs@data@values[g7Zone[[1]][,'cell']] <- inclProbs@data@values[g7Zone[[1]][,'cell']] * straw.props["g7"]
inclProbs@data@values[g8Zone[[1]][,'cell']] <- inclProbs@data@values[g8Zone[[1]][,'cell']] * straw.props["g8"]
inclProbs@data@values[g9Zone[[1]][,'cell']] <- inclProbs@data@values[g9Zone[[1]][,'cell']] * straw.props["g9"]
inclProbs@data@values[csZone[[1]][,'cell']] <- inclProbs@data@values[csZone[[1]][,'cell']] * straw.props["cs"]
inclProbs@data@values[czZone[[1]][,'cell']] <- inclProbs@data@values[czZone[[1]][,'cell']] * straw.props["cz"]
inclProbs@data@values[osZone[[1]][,'cell']] <- inclProbs@data@values[osZone[[1]][,'cell']] * straw.props["os"]
inclProbs@data@values[ozZone[[1]][,'cell']] <- inclProbs@data@values[ozZone[[1]][,'cell']] * straw.props["oz"]


plot(inclProbs)


writeRaster( inclProbs, file=paste0(paste(d.dir, paste("inclProbs" , study, platform, design.version, sep='-'), sep='/'), ".tif"), overwrite = TRUE)



