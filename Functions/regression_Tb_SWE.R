##############################################################################
# Title     : regression_Tb_SWE.R
# Purpose   : RLS for anual minimum Tb values and concurrent SWE
# Author    : Harold Llauca
##############################################################################

# regression_Tb_SWE(Y, X, method)
# Y      : anual minimum Tb matrix (years x basins or cells)
# X      : concurrent SWE matrix (years x basins or cells)
# method : Select the method used to process Tb data
#          'fbaw' - for basin scale
#          'gidw' - for point scale

lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

regression_Tb_SWE <- function(X, Y, method){
  
  if(method=='fbaw'){

     # Coef. correlation
     r   <- round(cor(X, Y), 2)
     
     # RLS model: y =a + bx 
     fit <- lm(Y~X) 
     a   <- round(coefficients(fit)[1],2)
     b   <- round(coefficients(fit)[2],2)
     p.value <- lmp(fit)
     
     # Model Error 
     error <- fit$fitted.values - Y
     rmse  <- round(sqrt(sum(error^2)/(length(error[!is.na(error)])-2)),2)
 
     Yreturn <- list(a=a, b=b, r=r, p.value=p.value, rmse=rmse)
     return(Yreturn)
  }

  if(method=='gidw'){

    r <- vector()
    a <- vector()
    b <- vector()
    rmse <- vector()
    p.value <- vector()
    
      for (i in 1:ncol(X)){
        if (sd(X[,i])==0){
          r[i]    <- NA
          a[i]    <- NA
          b[i]    <- NA
          error   <- NA
          rmse[i] <- NA
          p.value[i] <- NA
          
        } else {
          # Coef. correlation
          r[i]  <- cor(Y[,i], X[,i])
          
          # RLS model: y =a + bx 
          fit   <- lm(Y[,i] ~ X[,i])
          a[i]  <- round(coefficients(fit)[1],2)
          b[i]  <- round(coefficients(fit)[2],2)
          p.value[i] <- lmp(fit)
          
          # Model error
          error <- fit$fitted.values - Y[,i]
          rmse[i] <- round(sqrt(sum(error^2)/(length(error[!is.na(error)])-2)),2)
        }
      }
    
    Yreturn <- list(a=a, b=b, r=r, p.value=p.value, rmse=rmse)
    return(Yreturn)
  }
}