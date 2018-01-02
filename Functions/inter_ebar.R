##############################################################################
# Title     : inter_ebar.R
# Purpose   : Obtain parameters to plot ebars
# Author    : Harold Llauca
##############################################################################

# inter_ebar(X, y, inter, Xmin, Xmax)
# X      : Independent variable (X-axis) 
# y      : Dependent variable (y-axis)
# inter  : Intervals
# Xmax   : Maximum X value
# Xmin   : Minimum X value

inter_ebar = function(X, y, inter, Xmin, Xmax){
  
  p           <- vector()
  sd          <- vector()
  
  for (i in 1:length(inter)){
    i.mask  <- na.omit(X[X>=Xmin[i] & X<Xmax[i]])
    p[i]    <- mean(y[match(i.mask, X)], na.rm=T)
    sd[i]   <- sd(y[match(i.mask, X)], na.rm=T)
  }
  
  z           <- cbind(inter, p, p-sd, p+sd)
  colnames(z) <- c('int','mean','inf','sup')
  return(z)
} 