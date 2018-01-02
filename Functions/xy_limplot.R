xy_limplot <- function(xx, inf, sup) {
  min  <- floor(inf*min(xx, na.rm=T)/10)*10
  max  <- ceiling(sup*max(xx, na.rm=T)/10)*10
  return(c(min, max))
}
