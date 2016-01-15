
plotPop <- function (pop, maxind=50,wdl=5,...) {
  L<-ncol(pop)
  maxind<-min(maxind,nrow(pop))
  plot(c(0,L),c(0,maxind), type="n",xlab="loci",ylab="individuals",...)
  #text(1,1:N,popx,cex=1, pos=4)
  for(i in 1:maxind){
    for(j in 1:(L-1)) {
      lines(c(j,j+1),c(i,i), col=ifelse(pop[i,j],"red","blue"),lwd=wdl,lend="square")
      
    }
  }
}