##############################################################################
# Title     : array_idw.R
# Purpose   : Donwscaleing data by a IDW interpolation
# Author    : Harold Llauca
##############################################################################

# array_idw(x, y, data, data.x, data.y, beta)
# x		   : longitude of the point of interest
# y		   : latitude of the point of interest
# data   : Array [latitude x longitude x time] 
# data.x : Vector of longitude (from array data)
# data.y : Vector of latitude (from array data)
# beta   : ponderador del inverso de la distancia (e.g. 2)

array_idw <- function(x, y, data, data.x, data.y, beta){

  # Define distance
  dlat <- abs(data.y - y)
  dlon <- abs(data.x - x)
  
  # Define a point with minimum distance in latitude
  min_lat_1 <- which(dlat==min(dlat))
  min_lat_2 <- which(dlat==min(dlat[dlat!=min(dlat)]))
  
  # Define a point with minimum distance in longitude
  min_lon_1 <- which(dlon==min(dlon))
  min_lon_2 <- which(dlon==min(dlon[dlon!=min(dlon)]))
  
  # Extract the closer 04 grids
  A <- data[min_lat_1, min_lon_1,]
  B <- data[min_lat_2, min_lon_1,]
  C <- data[min_lat_2, min_lon_2,]
  D <- data[min_lat_1, min_lon_2,]
  
  # Calculate distance for the 04 closer grids
  dist_lat      <- c(dlat[c(min_lat_1, min_lat_2, min_lat_2, min_lat_1)])
  dist_lon      <- c(dlon[c(min_lon_1, min_lon_1, min_lon_2, min_lon_2)])
  distancia     <- (dist_lat^2+dist_lon^2)^0.5
  distancia_inv <- 1/distancia^beta
  
  # Interpolation by IDW
  data_idw <- (A*distancia_inv[1]+B*distancia_inv[2]+C*distancia_inv[3]+
               D*distancia_inv[4])/sum(distancia_inv)
  
  return(data_idw)
}