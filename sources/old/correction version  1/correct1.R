correct1<-function(n,Data,glossary){
#  generate correction instructions according to one glossary entry
  case<-glossary[n,]
  s2<-gregexpr(case$coded,Data)
  out0<-lapply(1:length(s2), function(x) data.frame(ind=x,where=s2[[x]],what=case$translated,offset=case$offset,stringsAsFactors=FALSE))
  out<-NULL
  for(x in 1:length(out0)) out<-rbind(out,out0[[x]])
  out[out$where!=-1,]
}