geRight<-function(code,indiv, n) code[indiv,n] %in% c(2,13)

geLeft<-function(code,indiv, n) code[indiv,n] %in% c(4,11)

crossingOver<-function(code,indiv, n) code[indiv,n] %in% c(3,12)

problem<-function(code,indiv, n) code[indiv,n] %in% c(5,6,9,10)