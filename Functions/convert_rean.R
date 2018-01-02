##############################################################################
# Title       : convert_rean.R
# Purpose     : Conviert SWE reanalysis from Matlab to R
# Author      : Harold Llauca
##############################################################################

# convert_rean(wd, file)
# wd    : Path where the file is located
# file  : Filename

convert_rean <- function(wd, file){
  
  # Verification of packages
  if("R.matlab" %in% rownames(installed.packages()) == F){
    install.packages("R.matlab", repos="http://r-forge.r-project.org")
  }
  # Load libreries
  library(R.matlab)

  # Set work directory and data
  setwd(wd)
  data      <- readMat(file)
  data.rean <- data$data.SWE                                # Data SWE
  data.rean <- replace(data.rean, data.rean=='NaN',NA)
  lat.rean  <- data$lat.vec                                 # Latitude
  lon.rean  <- data$lon.vec                                 # Longitude
  
  ## Save files in .Rdata
  save(list=c('data.rean','lat.rean','lon.rean'), file=paste(substr(file, 1, nchar(file)-4), '.Rda', sep=''))
  
  # Extract data in lat/lon/time
  nlat      <- dim(data.rean)[1]
  nlon      <- dim(data.rean)[2]
  nyr       <- dim(data.rean)[3]
  ndy       <- dim(data.rean)[4]
  time      <- nyr*ndy
  data.raw  <- array(NA, dim=c(nlat, nlon, time))
  for (i in 1:nyr){
    for (j in 1:ndy){
      k             <- j+ndy*(i-1)
      data.raw[,,k] <- data.rean[,,i,j]
    }
  }
  
  # Export results
  return(list(data=data.raw, lat=lat.rean, lon=lon.rean))
}
