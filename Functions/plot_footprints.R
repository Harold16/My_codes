##############################################################################
# Titulo        : plot.footprint.R
# Proposito     : Plotea las footprints nativas de AMSRE
# Autor         : Harold Llauca
# Entrada       : Coordenadas de las footprint
# Salida        : Footprints de AMSRE para una zona de interes
##############################################################################

plot.footprints <- function (centro, orientacion, area, x.lim, y.lim, x.at, y.at, id){

  # Cargar librerias
  if("plotrix" %in% rownames(installed.packages()) == F){
    install.packages("plotrix", repos="http://r-forge.r-project.org")
  } 
  library(plotrix)
  
  x.scale <- list(cex=0.7, at=x.at, labels=x.at/10^5)
  y.scale <- list(cex=0.7, at=y.at, rot=90, labels=y.at/10^6)

  plot(centro[,1], centro[,2], pch=15, cex=0.2, col='red', xaxt='n', yaxt='n',
       xlim=x.lim, ylim=y.lim, ann=F, asp=1)
  draw.ellipse(centro[,1], centro[,2], angle=orientacion, a=4000, b=7000,
               border='brown3', lwd=0.3, deg=F)
  plot(area, lwd=1.2, border='blue', add=T)
  axis(1, col='black', col.axis='black', cex.axis=0.7, at=x.at, labels=format(x.at/10^5, nsmall=2),
       lwd=.5)
  axis(2, col='black', col.axis='black', cex.axis=0.7, at=y.at, labels=format(y.at/10^6, nsmall=2),
       las=2, lwd=.5)
  mtext(expression('UTM Easting [10'^5*'m]'), side=1, cex=0.7, line=1.6)
  mtext(expression('UTM Northing [10'^6*'m]'), side=2, cex=0.7, line=1.7)
  abline(v=x.at, col="gray20", lty="dotted", lwd=.5)
  abline(h=y.at, col="gray20", lty="dotted", lwd=.5)
  mtext(paste('(',letters[id], ')', sep=''), side=3, line=-1., adj=0.03, font=2,
        cex=0.9)

}