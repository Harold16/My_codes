##############################################################################
# Title     : read_Tb_GIDW_Tb.R
# Purpose   : Processing Tb data in point scale
# Author    : Harold Llauca
##############################################################################

# read_Tb_GIW(path, name, ini, fin)
# path : Path where the AMSR-E data is stored
# name : Basin's name
# ini  : Start date
# fin  : End date
# number : Number of sub-basin

read_Tb_GIDW <- function(path, name, ini, fin, number){

  # Verification of packages
  if("matlabr" %in% rownames(installed.packages()) == FALSE){
    install.packages("matlabr", repos="http://r-forge.r-project.org")
  }
  if("R.matlab" %in% rownames(installed.packages()) == FALSE){
    install.packages("R.matlab", repos="http://r-forge.r-project.org")
  }
  if("lubridate" %in% rownames(installed.packages()) == FALSE){
    install.packages("lubridate", repos="http://r-forge.r-project.org")
  }
  if("zoo" %in% rownames(installed.packages()) == FALSE){
    install.packages("zoo", repos="http://r-forge.r-project.org")
  }
  
  # Load packages
  library(matlabr)
  library(R.matlab)
  library(lubridate)
  library(zoo)
  
  # Set a work directory
  setwd(path)
  filename <- paste('Tb_GIDW_', name,'_M', number,'.mat', sep='')
  
  # Run FBAW_modif.m in Matlab command promt
  # MATLAB work directory is required
  if (file.exists(filename)==F){
    run_matlab_script(paste(path,'GIDW_modif.m', sep='/'))
        while (!file.exists(filename)) {
          Sys.sleep(1)
  }}
  
  # Read .mat
  data.mat                      <- readMat(filename)
  data.raw                      <- as.data.frame(data.mat$data.tb.pto)
  data.raw[1,2:ncol(data.raw)]  <- NA # En Descenso el primer dato es erróneo
  colnames(data.raw)            <- c('Fecha', paste('Pto', 1:(ncol(data.raw)-1), sep='-'))
  
  # Convert dates from TAI93 (since midnight 01 ene 1993) to yyyy/mm/dd
  data.raw[,1]    <- as.Date(as.Date('1993/01/01', format='%Y/%m/%d')
                            + seconds_to_period(data.raw[,1]))
  
  # Create a vector of dates
  Yi              <- as.Date(ini)
  Yf              <- as.Date(fin)
  Dates           <- as.data.frame(cbind(seq(Yi, Yf, by='day'), NA))
  colnames(Dates) <- c('Fecha', 'N')
  Dates$Fecha     <- as.Date(Dates$Fecha)
    
  # Reconstruct Tb data
  data.fix    <- merge(data.raw, Dates, by='Fecha', all.y=TRUE)
  data.fix$N  <- NULL
  tb.gidw     <- as.matrix(data.fix[,-1])
    
  # Data with no leaps
  tb.gidw.nlp <- tb.gidw[format(data.fix$Fecha, "%m %d") != "02 29",]

  # Export results
  return(tb.gidw.nlp)
}