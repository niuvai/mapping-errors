doCorrect <- function (strpop, glossary) {
# corrects a population genotype using a glossary (dico)
# The coded version of the population genotypes has to be created outside the function (for the moment)
  corrected<-rep(paste(rep("9",nchar(strpop[1])),collapse=""),length(strpop))
#  Correct<-correction(strpop,dico)
  Correct<-NULL
  for(y in 1:nrow(glossary)) Correct<-rbind(Correct,correct1(y,strpop,glossary))
  Correct$times<-Correct$length/nchar(Correct$what)
  for(x in 1:nrow(Correct)){
    pos<-Correct$where[x]+Correct$offset[x]
    len<-Correct$length[x]
    substr(corrected[Correct$ind[x]],pos,pos+len)<-paste(rep(Correct$what[x],Correct$times[x]),collapse = "")
  }
  corrected
}
