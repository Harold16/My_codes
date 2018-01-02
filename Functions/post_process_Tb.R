##############################################################################
# Title     : post_process_Tb.R
# Purpose   : Post-processing Tb data in basin or point scale
# Author    : Harold Llauca
##############################################################################

# process_tb(Y, X, ma, ny, method)
# Y      : Tb data vector (fbaw) or matrix (gidw)
# X      : SWE data vector (fbaw) or matrix (gidw)
# ma     : Lag for moving average smoothing
# ny     : Number of years to process
# method : Select the method to process Tb from AMSR-E.
#          'fbaw' - for basin scale
#          'gidw' - for point scale

post_process_Tb <- function(Y, X, ma, ny, method){
  
  # Verification of packages
  if("zoo" %in% rownames(installed.packages()) == FALSE){
    install.packages("zoo", repos="http://r-forge.r-project.org")
  }
  # Load packages
  library(zoo)

  
  if (method=='fbaw'){
    
    # Smoothing data (Moving Average)
    Yma       <- rollapply(Y, ma, mean, na.rm=T, fill=NA)
    
    # Obtaining annual minimum Y values (hy)
    Ymin      <- vector()
    for (i in 1:ny){
      ini     <- 1+365*(i-1)
      fin     <- 365+365*(i-1) 
      Ymin[i] <- min(Yma[ini:fin], na.rm=T)
    }
    
    # Obtaining annual maximum X values (hy)
    Xmax       <- vector()
    for (i in 1:ny){
      ini      <- 1+365*(i-1)
      fin      <- 365+365*(i-1) 
      Xmax[i]  <- max(X[ini:fin], na.rm=T)
    }
    
    # Obtaining concurrent X values
    index     <- match(Ymin, Yma)
    Xcon      <- X[index]
    
    index.2   <- match(Xmax, X)
    
    # Calculate delta values for position [-] and X values [%]
    dI <- index.2 - index
    dX <- ((Xcon - Xmax)/Xmax)*100

    # Results
    Yreturn   <- list(Yma=Yma, Ymin=Ymin, Xcon=Xcon, dI=dI, dX=dX, Index=index)
    return(Yreturn)
  }

  if (method=='gidw'){
    
    # Smoothing data (Moving Average)
    Yma       <- matrix(ncol=ncol(Y), nrow=365*ny)
    for (j in 1:ncol(Y)){
      Yma[,j] <- rollapply(Y[,j], ma, mean, na.rm=T, fill=NA)
    }
    
    # Obtainimg anual minimum Y values (hy)
    Ymin      <- matrix(ncol=ncol(Yma), nrow=ny)
    for (k in 1:ncol(Yma)){
      for (ii in 1:ny){
        ini        <- 1+365*(ii-1)
        fin        <- 365+365*(ii-1) 
        Ymin[ii,k] <- min(Yma[ini:fin, k], na.rm=T)
      }
    }
    
    # Obtaining concurrent SWE values
    index <- matrix(ncol=ncol(Yma), nrow=ny)
    Xcon  <- matrix(ncol=ncol(Yma), nrow=ny)
    for (jj in 1:ncol(Yma)){
      index[,jj] <- match(Ymin[,jj], Yma[,jj])
      Xcon[,jj]  <- swe.regpt[index[,jj],jj]
    }
    
    # Results
    Yreturn <- list(Yma=Yma, Ymin=Ymin, Xcon=Xcon)
    return(Yreturn)
  }
}