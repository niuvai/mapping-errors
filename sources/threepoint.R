threePoint<-function(dist,n){
  # Donne les deux segments et l'erreur
  # utilisé seulement au début et à la fin!
  A<-dist[n-1,n]
  B<-dist[n,n+1]
  C<-dist[n-1,n+1]
  error<-A+B-C
  distA<-C-B
  distB<-C-A
  c(error=error,recL=distA,recR=distB)
}