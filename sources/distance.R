distance<-function(pop,i,j){
  # Computes recombination distances. This new version accepts NAs
  # à nouveau..
  sum(pop[,i]!=pop[,j],na.rm=TRUE)/sum(!(is.na(pop[,i])|is.na(pop[,j])))
}