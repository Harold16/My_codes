##############################################################################
# Title     : read_Tb_FBAW_Tb.R
# Purpose   : Processing Tb data in basin scale
# Author    : Harold Llauca
##############################################################################

# read_Tb_FBAW(path, name, ini, fin)
# path : Path where the AMSR-E data is stored
# name : Basin's name
# ini  : Start date
# fin  : End date

read_Tb_FBAW <- function(path, name, ini, fin, number ){

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
  filename <- paste('Tb_FBAW_', name, '_M', number,'.mat', sep='')
  
  # Run FBAW_modif.m in Matlab command promt
  # MATLAB work directory is required
  if (file.exists(filename)==F){
    run_matlab_script(paste(path,'FBAW_modif.m', sep='/'))
      while (!file.exists(filename)) {
          Sys.sleep(1)
  }}

  # Read .mat
  data.mat            <- readMat(filename)
  data.raw            <- as.data.frame(data.mat$high.Tb.FBAW)
  colnames(data.raw)  <- c('Fecha', 'Tb', 'STD')
  data.raw[1,2:3]     <- NA # The first value is wrong
    
  # Convert dates from TAI93 (since midnight 01 ene 1993) to yyyy/mm/dd
  data.raw[,1]        <- as.Date(as.Date('1993/01/01', format='%Y/%m/%d')
                           + seconds_to_period(data.raw[,1]))
  
  # Create a vector of dates
  Yi              <- as.Date(ini)
  Yf              <- as.Date(fin)
  Dates           <- as.data.frame(cbind(seq(Yi, Yf, by='day'), NA))
  colnames(Dates) <-c('Fecha', 'N')
  Dates$Fecha     <- as.Date(Dates$Fecha)
    
  # Reconstruct Tb data
  data.fix    <- merge(data.raw, Dates, by='Fecha', all.y=T)
  data.fix$N  <- NULL
  tb.fbaw     <- as.numeric(data.fix[,2])
  tb.fbaw.std <- as.numeric(data.fix[,3])
  
  # Data with no leaps
  tb.fbaw.nlp <- tb.fbaw[format(data.fix$Fecha, "%m %d") != "02 29"]
  # tb.fbaw.std <- tb.fbaw.std[format(data.fix$Fecha, "%m %d") != "02 29"]
  
  # Export results
  return(tb.fbaw.nlp)
}
