recode<-function(indiv,n,base=2){
  #convert genotype at four loci into a numner 
  indiv[n+3]+2*(indiv[n+2]+2*(indiv[n+1]+2*(indiv[n])))
}

toCode<-function(pop)
c1<-t(sapply(1:nrow(pop), function(y) t(sapply(1:(ncol(pop)-4),function(x) recode(pop[y,],x)))))


toStrings<-function(Data)
	switch(class(Data[1]),
		logical=apply(Data,1, function(x) paste(as.numeric(x), collapse="")),
		numeric=apply(Data,1, function(x) paste(as.hexmode(x), collapse=""))
	)

toMat <- function (corr) {
  # reverse of toStrings
  corrMat<-t(sapply(1:length(corr),function(x) strsplit(corr[x],"")[[1]],simplify=TRUE))
  corrMat[corrMat=="9"]<-NA
}