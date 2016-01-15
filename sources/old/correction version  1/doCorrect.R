doCorrect <- function (popstr, codestr, dico) {
# corrects a population genotype using a glossary (dico)
# The coded version of the population genotypes has to be created outside the function (for the moment)
  corrected<-rep(paste(rep("9",nchar(popstr[1])),collapse=""),length(popstr))
  Correct<-correction(codestr,dico)
  # Correct
  for(x in 1:nrow(Correct)){
    pos<-Correct$where[x]+Correct$offset[x]
    len<-nchar(Correct$what[x])
    substr(corrected[Correct$ind[x]],pos,pos+len)<-Correct$what[x]
  }
  corrected
}
