##EJERCICIO 3
library(quantmod)

#EJERCICIO 3  

GB <- function(r,sigma,T,n,s0){
  dt=T/n
  t=seq(0,T,by=dt)
  bt=rnorm(n,mean=0,sd=1)
  s=c(s0*(exp(r-sigma^2/2)*dt)+sigma*bt)
  st=cumsum(s)
}

x2<-GB(0,1,1/12,200,10)
m<-10
n<-200
x1
ppgm<-x1<m
sppgm<-sum(ppgm)
pgm<-sppgm/n
pgm