##############################################################################
# Title     : read_rean.R
# Purpose   : Read and save reanalysis SWE
# Author    : Harold Llauca
##############################################################################

# read_rean(rean.path, shp.path, rean.file, shp.file, area.name, number)
# rean.path : Path where reanalysis SWE data is located
# shp.path  : Path where data GIS is located
# rean.file : SWE renanalysis file
# shp.file  : Shapefile to open
# area.name : Basin names
# number    : Numer ID of the sub-basin 


read_rean <- function(rean.path, shp.path, rean.file, shp.file, area.name, number){

  # Verification of packages
  if("rgdal" %in% rownames(installed.packages()) == FALSE){
    install.packages("rgdal", repos="http://r-forge.r-project.org")
  }
  
  # Load packages
  library(rgdal)

  # Functions path
  fun <- 'C:/HAROLD/Tesis/Functions/'
  setwd(fun)
  
  # Read reanalysis in array [lat x lon x time]
  source('convert_rean.R')
  rean      <- convert_rean(rean.path, rean.file)
  data.raw  <- rean$data
  lat       <- rean$lat
  lon       <- rean$lon
  
  # Subset reanalysis
    # Extract extention of the study area
    high.area <- readOGR(paste(shp.path, shp.file, sep=''))
    sdf       <- summary(high.area)
    xy        <- sdf$bbox
    min.lon   <- xy[1,1]
    max.lon   <- xy[1,2]
    min.lat   <- xy[2,1]
    max.lat   <- xy[2,2]
    
    # Cut data
    lon.rean  <- t(lon)
    lat.rean  <- t(lat)
    lon.reg   <- subset(lon.rean, (lon.rean> min.lon) & (lon.rean< max.lon))
    lat.reg   <- subset(lat.rean, (lat.rean> min.lat) & (lat.rean< max.lat))
    swe.reg   <- data.raw[match(lat.reg, lat.rean), match(lon.reg, lon.rean),]
    nlat.reg  <- length(lat.reg)
    nlon.reg  <- length(lon.reg)
  
    # Calculate SWE mean
    source('create_raster.R')
    m         <- matrix(1, ncol=nlon.reg, nrow=nlat.reg)
    limites   <- c(range(lon.reg), range(lat.reg))
    ras       <- create_raster(m, 0.05, limites, high.area, F, F)
    mask      <- which(!is.na(as.vector(as.matrix(mask(ras, high.area)))), T)
    swe.mean  <- vector()
    for (i in 1:dim(swe.reg)[3]){
      swe         <- as.vector(swe.reg[,, i])
      swe.mean[i] <- mean(swe[mask], na.rm=T)  
    }
  
  # # Downscale SWE by a IDW interpolation
  #   source('array_idw.R')
  #   beta    <- 2
  #   est     <- read.table(pts.file)
  #   swe.est <- matrix(ncol=nrow(est), nrow=dim(swe.reg)[3])
  #   for (i in 1:nrow(est)){
  #     lat.est     <- est[i,2]
  #     lon.est     <- est[i,1]
  #     swe.est[,i] <- array_idw(lon.est, lat.est, swe.reg, lon.reg, lat.reg, beta)
  #   }
  
  # Convert data to matrix [time x cells]
  swe.regpt <- matrix(ncol=nlat.reg*nlon.reg, nrow=dim(swe.reg)[3])
  for (i in 1:nlon.reg){
    for (j in 1:nlat.reg){
      w             <- j+(i-1)*nlat.reg
      swe.regpt[,w] <- swe.reg[j,i,]
    }
  }
  
  # Extract coordinates of grid points
  ptos.rean <-cbind(rep(lon.reg, each=nlat.reg), rep(lat.reg, nlon.reg))
  
  # Export results
  if (number == 0){
    save(list=c('swe.reg', 'swe.mean', 'swe.regpt', 'lat.reg', 'lon.reg'),
         file=paste('Rean_SWE_', area.name,'.Rda', sep=''))
    write.table(ptos.rean, paste('Grillas_SWE_', area.name,'.txt', sep=''),
                sep='\t', row.names=F, col.names=F)
  } else{
    save(list=c('swe.reg', 'swe.mean', 'swe.regpt', 'lat.reg', 'lon.reg'),
         file=paste('Rean_SWE_', area.name,'_M', number,'.Rda', sep=''))
    # write.table(ptos.rean, paste('Grillas_SWE_', area.name,'_M', number,
    #             '.txt', sep=''), sep='\t', row.names=F, col.names=F)
  }
}