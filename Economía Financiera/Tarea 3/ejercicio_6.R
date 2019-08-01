library(quantmod)
#EJERCICIO 6

YF<-function(x){
  z<-x[,1]
  row<-sample(1:length(z), 1)
  rand<-x[row,]
  Adjt<-rand[,6]
  Adjt2<-x[row+1,6]
  AdjtDF<-as.matrix(Adjt)
  Adjt2DF<-as.matrix(Adjt2)
  p<-AdjtDF<Adjt2
  p
}
#PROBEMAS CON LA FUNCION
f2 <- function(n.rep, x){
  sar <- replicate(n.rep, YF(x))
}

r1<-function(x){
  replicados <- sample(f2(500,x), size = 10000, replace=TRUE)
  repis <- sum(replicados)/10000
  repis
}



getSymbols("OIL")
x<-OIL
r1(x)

getSymbols("GOLD")
x2<-GOLD
r1(x2)

getSymbols("CL")
x3<-CL
r1(x3)

getSymbols("SILVER")
x4<-SILVER
r1(x4)

getSymbols("GSPC")
x5<-GSPC
r1(x5)


