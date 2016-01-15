single<-function(pop){
  # Counts genotypes  
  hetero<-apply(pop,2,sum)
  homo<-nrow(pop)-hetero
  rbind(hetero,homo)
}