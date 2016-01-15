missingData<-function(pop,missDat){
  # generates missing data
  
  miss<-matrix(sample(g,nrow(pop)*ncol(pop),replace=TRUE,prob=c(missDat,1-missDat)),nrow=nrow(pop))
  t(sapply(1:nrow(pop), function(x) sapply(1:ncol(pop), function(y) ifelse(miss[x,y],NA,pop[x,y]))))
}
