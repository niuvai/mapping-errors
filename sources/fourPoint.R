fourPoint<-function(dist,n){
  # Evaluate actual recombination rate between n+1 and n+2
  # As well as genotypic errors  at n+1 and n+2
  A<-dist[n,n+1]
  B<-dist[n+1,n+2]
  C<-dist[n+2,n+3]
  D<-dist[n,n+2]
  E<-dist[n+1,n+3]
  F<-dist[n,n+3]
  rec<-(F+B-A-C)/2
  g1<-(A+B-D)/2
  g2<-(B+C-E)/2
  c(rec=rec,g1=g1,g2=g2)
}
