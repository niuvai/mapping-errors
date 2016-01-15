correction<-function(Data,glossary){
# Produces correction instructions according to a glossary
  out<-NULL
  for(y in 1:nrow(glossary)){
    out<-rbind(out,correct1(y,Data,glossary))
  }
  out
}
