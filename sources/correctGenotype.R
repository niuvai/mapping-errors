correctGenotype <- function (noisy) {
  # create a corrected version of noisy genotyping data
  # The rules at the limits need reworking. 
  # I had to cancel rules 5 to 8: the resulting chains look nice *but* they are terribly bad in terms of distance estimation. 
  rule<-NULL
  rule<-rbind(rule,cbind(query="0{3,}(1{0,2}0{3,})*",problem="1",correct="0")) # no errors or isolated errors
  rule<-rbind(rule,cbind(query="1{3,}(0{0,2}1{3,})*",problem="0",correct="1")) # no errors or isolated errors
  rule<-rbind(rule,cbind(query="0{3,}10{1,2}10{3,}" ,problem="1",correct="0")) # tandem errors
  rule<-rbind(rule,cbind(query="1{3,}01{1,2}01{3,}" ,problem="0",correct="1")) # tandem errors
#  rule<-rbind(rule,cbind(query="^0{2,4}1+" ,problem="",correct="")) # beginning (don't correct)
#  rule<-rbind(rule,cbind(query="^1{2,4}0+" ,problem="",correct="")) # beginning (don't correct)
#  rule<-rbind(rule,cbind(query="1+0{2,4}$" ,problem="",correct="")) # end (don't correct)
#  rule<-rbind(rule,cbind(query="0+1{2,4}$" ,problem="",correct="")) # end (don't correct)
  
  corr<-rep(paste(rep("9",nchar(noisy[1])),collapse=""),length(noisy))
  
  for(i in 1:nrow(rule)) {
    found <- gregexpr(rule[i,"query"],noisy,perl=TRUE)
    matches <- regmatches(noisy,found)   # ce qui a été repéré
    condition <- sapply(matches,length) != 0
    if (sum(condition) > 0) {
      for (x in 1:length(matches[condition]))
        regmatches(matches[condition][[x]],gregexpr(rule[i,"problem"],matches[condition][[x]],perl=TRUE)) <- rule[i,"correct"]
      regmatches(corr,found) <- matches
    }
  }
  corr
}
diagnostic<-function(origin,noisy,corr){
  # identifies remaining problems
  filter<-grepl("9+",corr)
  u<-lapply(1:length(origin), function(x) rbind(true=origin[x],noisy=noisy[x],corrected=corr[x]))
  u[filter]
}
unknown<-function(corr){
  # calculates the percentage of unsolved cases
  unk<-regmatches(corr,gregexpr("9+",corr))
  sunk<-sum(sapply(unk,function(x) sum(nchar(x))))
  spos<-sum(nchar(corr))
  paste(sunk/spos*100,"% unknown genotypes")
}