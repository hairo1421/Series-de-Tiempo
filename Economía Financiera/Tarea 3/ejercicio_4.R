library(quantmod)

#EJERCICIO 4

n4<-30
fprob<-function(x,n){
  sb1<-na.omit(diff(x))
  sb<-sb1[1:30]
  ppsb<-sb<0
  pb<-sum(ppsb)/n4
  ps<-1-pb
  pb
  ps
  matrix(c(pb,ps),nrow=1,ncol=2,dimnames = list(c("prob"),c("ProbBajar","ProbSubir")))
}


xa<-GB(0,1,1/365,10000,100)
fprob(xa,30)
xb<-GB(0,.2,1/365,10000,100)
fprob(xb,30)
xc<-GB(.1,.2,1/365,10000,100)
fprob(xc,30)
xd1<-GB(0,1,1/180,10000,100)
fprob(xd1,30)
xd2<-GB(0,.2,1/180,10000,100)
fprob(xd2,30)
xd3<-GB(.1,.2,1/180,10000,100)
fprob(xd3,30)