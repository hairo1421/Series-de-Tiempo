library(quantmod)

##EJERCICIO2

brow.a<- function(r,sigma,T,n,s0){
  dt=T/n
  t=seq(0,T,by=dt)
  zt=rnorm(n,mean=0,sd=1)
  s=c(s0,r*dt+sigma*sqrt(dt)*zt)
  st=cumsum(s)
  
}

#A
x1<-brow.a(0,1,1/12,200,10)
n=200

pp<-x1<0
spp<-sum(pp)
p<-spp/n
p

pp1<-x1<1
spp1<-sum(pp1)
p1<-spp1/n
p1

pp2<-x1<9
spp2<-sum(pp2)
p2<-spp2/n
p2

pp3<-x1<10
spp3<-sum(pp3)
p3<-spp3/n
p3


