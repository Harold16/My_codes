count_clusters <- function(position, n){
  
  # n=3
  # position=fit$cluster
  
  count <- vector()
  for (i in 1:n){
    count[i] <-length(which(position==i))
  }
  
  f       <- cbind(1:n, count)
  f.order <- f[order(f[,2], decreasing=T),]
  f.clus  <- f.order[,1]
  f.count <- f.order[,2]
  
  Yreturn <- list(f.clus=f.clus, f.count=f.count)
  return(Yreturn)
}