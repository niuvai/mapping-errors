GenotypingErrors<-function(pop,genotErr){
  # genotyping errors une vieille version
  
  err<-matrix(sample(g,nrow(pop)*ncol(pop),replace=TRUE,prob=c(genotErr,1-genotErr)),nrow=nrow(pop))
  pop!=err # TT->F, TF-<T, FT->T and FF->F
}