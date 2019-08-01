library(quantmod)

## Ejercicio 1

brow <- function(r,sigma,T,n,s0){
  dt=T/n
  t=seq(0,T,by=dt)
  zt=rnorm(n,mean=0,sd=1)
  s=c(s0,r*dt+sigma*sqrt(dt)*zt)
  st=cumsum(s)
}


dons <- function(r,sigma,T,n,s0){
  dt=T/n
  t=seq(0,T,by=dt)
  zt=rnorm(n,mean=0,sd=1)
  s=c(s0,r*dt+sigma*sqrt(dt)*zt)
  st=cumsum(s)/sqrt(n)
  sf=sum(s)+s0
}

dons2 <- function(n){
  zt <- rnorm(n)
  wt <- cumsum(zt/sqrt(n))
  plot(wt, type='l', col = "blue")
}
dons2(3000)