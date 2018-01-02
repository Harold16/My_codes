##############################################################################
# Titulo        : plot.swath.R
# Proposito     : Crea una figura del swath de AMSRE
# Autor         : Harold Llauca
# Entrada       : Coordenadas del swath y punto de interes en (lat, lon) 
# Salida        : Figura del globo terrestre y swath de AMSRE-L2A
##############################################################################

plot.swath<-function(swath, point, id){
# plot.swath(coordenadas, centroide)
  # swath : Matriz que contiene la latitud y longitud del swath (lat, lon)
  # point : vector Matriz que contiene la latitud y longitud del punto
  #         de interes (longitud, latitud)
  
  # Cargar librerias
  if("globe" %in% rownames(installed.packages()) == F){
    install.packages("globe", repos="http://r-forge.r-project.org")
  } 
  library(globe)
  
  # Generar figura
  globeearth(eye=list(-60,-25), col='blue', lwd=0.8)
  globedrawlat(lat=seq(-90, 90, 30), col='grey', lwd=0.5)
  globedrawlong(lon=seq(-180, 180, 30), col='grey', lwd=0.5)
  globelines(swath, col='red', lwd=1.3, bg="gray")
  globepoints(point, pch='x', cex=1.8)
  mtext(paste('(',letters[id], ')', sep=''), side=3, line=-1.3, adj=0.05, font=2, cex=0.9)
  mtext(expression('AMSR-E L2A 1'^'st'~'Sep 2005'), side=1, line=0, adj=0.5, font=1, cex=0.8)
}
