simPop<-function(size,locusNum,distCM){
  # Suggested values
  # size<-300         #(300) individuals
  # locusNum<-100     #(100) loci
  # distCM<-1         #(1) centimorgan
  g<-c(TRUE,FALSE)  # True: heterozygote, False: homozygote
  recRate<-distCM/100
  ################## begin simulation #############################
  # First locus
  G<-sample(g,size,replace=TRUE)
  #Recombination events
  rec<-matrix(sample(g,size*(locusNum-1),replace=TRUE,prob=c(recRate,1-recRate)),nrow=size)
  # propagate recombination status
  rec<-cbind(TRUE,rec)
  for(i in 2:locusNum) rec[,i]<-rec[,i-1]!=rec[,i]
  #complete genotype
  pop<-sapply(1:locusNum,function(x) rec[,x]== G) 
  pop
}