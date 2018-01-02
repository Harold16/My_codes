##############################################################################
# Title     : create_raster.R
# Purpose   : Create a raster and reproject and mask 
# Author    : Harold Llauca
##############################################################################

# create_raster(data, res, lim, area, reproj, mask)
# data   : data matrix
# res    : raster resolution
# lim    : raster boundary
# area   : shapefile to mask raster (only if mask is TRUE)
# reproj : TRUE (T) or FALSE (F) to reproject raster to UTM
# mask   : TRUE (T) or FALSE (F) to mask raster

create_raster <-function(data, res, lim, area, reproj, mask){

  # Verification of packages
  if("raster" %in% rownames(installed.packages()) == FALSE){
    install.packages("raster", repos="http://r-forge.r-project.org")
  }
  # Load packages
  library(raster)
  
  # Create raster in WGS84 (GEO)
  raster          <- raster(data)
  xmin            <- lim[1]-0.5*res
  xmax            <- lim[2]+0.5*res
  ymin            <- lim[3]-0.5*res
  ymax            <- lim[4]+0.5*res
  crs(raster)     <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' #EPSG 4326
  extent(raster)  <- c(xmin, xmax, ymin, ymax)
  raster          <- crop(raster, area)
  raster.mask     <- mask(raster, area)
    if (mask==T){ return(raster.mask)
    } else{ return(raster)}
  
  if (reproj==T){
  # Reproject to UTM 19S
  srs             <- '+proj=utm +zone=19 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
  raster.proj     <- projectRaster(raster, crs=srs, method='ngb')
      if (mask==T){ return(raster.mask)
      } else{ return(raster.proj)}
 }
}