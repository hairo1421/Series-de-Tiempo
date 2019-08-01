######Datos ﬁnancieros #########
#instalando paqueteria 
install.packages("quantmod")
#descargando datos, (SP500)
library(quantmod)
library(forecast)
library(astsa)
#simbolos seleccionados
#Dow Jones Industrial Average--^DJI
#Nikkei 225 --^N225 (tokmo japsn)
#AEX-INDEX --^AEX (pamses bajos)
#IPC---^MXX
#CAC 40 (^FCHI) paris 

#######################EJERCICIO 1#############################

#EVALUANDO HECHOS ESTILIZADOS PARA GSPC
getSymbols("^GSPC")
#generando variable
sp500<-get("GSPC")

#se seleccionan los precios ajustados
sp500 <- Ad(sp500)

#h:1 precios de las acciones impredecibles
par(mfrow=c(2,1))
ret.sp500<-diff(log(sp500))
plot(ret.sp500, main="retornos sp500")
plot(sp500, main="sp500")
#h:2 retornos no correlacionado
acf(na.omit(ret.sp500))
pacf(na.omit(ret.sp500))
tsdisplay(ret.sp500, lag.max = 20)
#h:3 retornos al cuadrado correlacionados
r.sp500<-ret.sp500^2 
acf(na.omit(r.sp500))
pacf(na.omit(r.sp500))
tsdisplay(na.omit(r.sp500))
#h:4 closters de volatilidad
par(mfrow=c(3,1))
plot(na.omit(ret.sp500,main="retornos sp500")) 
plot(na.omit(r.sp500,main="retornos al cuadrado sp500")) 
plot(na.omit(abs(ret.sp500),main="retornos en absolutos sp500")) 
#podemos observar que tanto en los retornos, sus cuadrados y en valores absolutos es notable los closters
par(mfrow=c(2,1))
acf(na.omit(ret.sp500,main="retornos sp500"),lag.max = 500) 
acf(na.omit(r.sp500,main="retornos al cuadrado sp500"),lag.max = 500)
#podemos observar significancia en lags alejados, se nota volatilidad por percistencia de autocorrelacisn
#la varianza puede estar condicionada a sus valores pasados lo que determina closters
umbral <- quantile(na.omit(ret.sp500),0.95)
menor.umbr<-as.numeric(ret.sp500>umbral)
agrupa.rets<-menor.umbr
as.ts(agrupa.rets)
par(mfrow=c(1,1))
plot(agrupa.rets)

#LOS VALORES SE AGLOMERAN
umb<-function(IC){
  umbral <- quantile(na.omit(ret.sp500),IC)
  menor.umbr<-as.numeric(ret.sp500>umbral)
  agrupa.rets<-menor.umbr
  as.ts(agrupa.rets)
  par(mfrow=c(1,1))
  plot(agrupa.rets)
}

umb(.99)

#LOS VALORES SE AGLOMERAN

#h:5 leptocurtosis
par(mfrow=c(1,1))
plot(density(na.omit(ret.sp500, main="Densidad de los retornos del sp500")))
z<-seq(-5,5,len=1000)
x<-dnorm(z,mean=mean(ret.sp500,na.rm=T),sd=sd(ret.sp500,na.rm=T))
lines(z,x,col='red',lty=2,lwd=2)
#qq plot para determinar fat tails
w<-na.omit(coredata(ret.sp500))
qqnorm(w)
qqline(w,col='red',lty=2)
#se observa informacisn a los extremos el cual es factor para tener colas anchas

#h:6 leverage efect
rets<-function(a){
  b<-diff(log(Ad(a)))
  return(b)
}
dato<-na.omit(ret.sp500)
mas<-apply.daily(dato,function(a) max(a,0))
menos<-apply.daily(dato,function(a) -min(a,0))
abs<-apply.daily(dato,function(a) abs(a))
lag2.plot(mas,abs,8)
lag2.plot(menos,abs,8)
#choques negativos y psitivos son asimetricos, los positivos hacen que los
#retornos varien menos que los negativos

#justificaci??n:??Retornos se modelan mejor con ruido blanco debnil o fuerte?
#Ruido blanco debil
#Ya que su funci??n de autocorrelaci??  de los retornos logaritmicos, #
#nos muestra valores significativos en sus lags.
#recoredemos que independencia nos indica no correlaci??, en el cual est?? no es el caso. 
#Con el Plot de la serie podemos ver como un valor elevado le precede  otro elevado, imdicandonos
#un proceso con memoria a lo inmediato, posiblemente por la correlaci??n.

#EVALUANDO HECHOS ESTILIZADOS PARA DJI
getSymbols("^DJI") 
#generando variable
djon<-get("DJI")
#se seleccionan los precios ajustados
djon<- Ad(djon)
#h:1 precios de las acciones impredecibles
par(mfrow=c(2,1))
ret.djon<-diff(log(djon))
plot(ret.djon, main="retornos down jones")
plot(djon, main="down jones")
#h:2 retornos no correlacionado
par(mfrow=c(2,1))
acf(na.omit(ret.djon))
pacf(na.omit(ret.djon))
tsdisplay(ret.djon, lag.max = 20)
#h:3 retornos al cuadrado correlacionados
r.djon<-ret.djon^2 
par(mfrow=c(2,1))
acf(na.omit(r.djon))
pacf(na.omit(r.djon))
tsdisplay(na.omit(r.djon))
#h:4 closters de volatilidad
par(mfrow=c(3,1))
plot(na.omit(ret.djon,main="retornos down jones")) 
plot(na.omit(r.djon,main="retornos al cuadrado down jones")) 
plot(na.omit(abs(ret.djon),main="retornos en absolutos down jones")) 
#podemos observar que tanto en los retornos, sus cuadrados y en valores absolutos es notable los closters
par(mfrow=c(2,1))
acf(na.omit(ret.djon,main="retornos down jones"),lag.max = 500) 
acf(na.omit(r.djon,main="retornos al cuadrado down jones"),lag.max = 500)
#podemos observar significancia en lags alejados, se nota volatilidad por percistencia de autocorrelacisn
#la varianza puede estar condicionada a sus valores pasados lo que determina closters

umbral2 <- quantile(na.omit(ret.djon),0.95)
menor.umbr2<-as.numeric(ret.djon>umbral2)
agrupa.rets2<-menor.umbr2
as.ts(agrupa.rets2)
par(mfrow=c(1,1))
plot(agrupa.rets2)
#LOS VALORES SE AGLOMERAN 

umb2<-function(IC){
  umbral2 <- quantile(na.omit(ret.djon),IC)
  menor.umbr2<-as.numeric(ret.djon>umbral2)
  agrupa.rets2<-menor.umbr2
  as.ts(agrupa.rets2)
  par(mfrow=c(1,1))
  plot(agrupa.rets2)
}

umb2(.99)
#LOS VALORES SE AGLOMERAN EN LAS PRIMERAS OBSERVACIONES QUE SALEN DEL INTERVALO

#h:5 leptocurtosis
par(mfrow=c(1,1))
plot(density(na.omit(ret.djon, main="Densidad de los retornos del down jones")))
z2<-seq(-5,5,len=1000)
x2<-dnorm(z2,mean=mean(ret.djon,na.rm=T),sd=sd(ret.djon,na.rm=T))
lines(z2,x2,col='red',lty=2,lwd=2)
#qq plot para determinar fat tails
w2<-na.omit(coredata(ret.djon))
qqnorm(w2)
qqline(w2,col='red',lty=2)
#se observa informacisn a los extremos el cual es factor para tener colas anchas

#h:6 leverage efect
rets<-function(a){
  b<-diff(log(Ad(a)))
  return(b)
}
dato2<-na.omit(ret.djon)
mas2<-apply.daily(dato2,function(a) max(a,0))
menos2<-apply.daily(dato2,function(a) -min(a,0))
abs2<-apply.daily(dato2,function(a) abs(a))
lag2.plot(mas2,abs2,8)
lag2.plot(menos2,abs2,8)
#choques negativos y psitivos son asimetricos, los positivos hacen que los
#retornos varien menos que los negativos

#justificaci??n:??Retornos se modelan mejor con ruido blanco debnil o fuerte?
#Ruido blanco debil
#Ya que su funci??n de autocorrelaci??  de los retornos logaritmicos, #
#nos muestra un valo significativos en su lag 2.
#recoredemos que independencia nos indica no correlaci??, en el cual est?? no es el caso. 
#Con el Plot de la serie podemos ver como un valor elevado le precede  otro elevado, imdicandonos
#un proceso con memoria a lo de ayer , posiblemente por la correlaci??n.

#EVALUANDO HECHOS ESTILIZADOS PARA ^N225
getSymbols("^N225")
#generando variable
n_225<-get("N225")
#se seleccionan los precios ajustados
n_225 <- Ad(n_225)
#h:1 precios de las acciones impredecibles
par(mfrow=c(2,1))
ret.n225<-diff(log(n_225))
plot(ret.n225, main="retornos N225")
plot(n_225, main="N225")
#h:2 retornos no correlacionado
acf(na.omit(ret.n225))
pacf(na.omit(ret.n225))
tsdisplay(ret.n225, lag.max = 20)
#h:3 retornos al cuadrado correlacionados
r.n225<-ret.n225^2 
par(mfrow=c(2,1))
acf(na.omit(r.n225))
pacf(na.omit(r.n225))
tsdisplay(na.omit(r.n225))
#h:4 closters de volatilidad
par(mfrow=c(3,1))
plot(na.omit(ret.n225,main="retornos N225")) 
plot(na.omit(r.n225,main="retornos al cuadrado N225")) 
plot(na.omit(abs(ret.n225),main="retornos en absolutos N225")) 
#podemos observar que tanto en los retornos, sus cuadrados y en valores absolutos es notable los closters
par(mfrow=c(2,1))
acf(na.omit(ret.n225,main="retornos N225"),lag.max = 500) 
acf(na.omit(r.n225,main="retornos al cuadrado N225"),lag.max = 500)
#podemos observar significancia en lags alejados, se nota volatilidad por percistencia de autocorrelacisn
#la varianza puede estar condicionada a sus valores pasados lo que determina closters

umbral3 <- quantile(na.omit(ret.n225),.95)
menor.umbr3<-as.numeric(ret.n225>umbral3)
agrupa.rets3<-menor.umbr3
as.ts(agrupa.rets3)
par(mfrow=c(1,1))
plot(agrupa.rets3)
#LOS VALORES SE AGLOMERAN

umb3<-function(IC){
  umbral3 <- quantile(na.omit(ret.n225),IC)
  menor.umbr3<-as.numeric(ret.n225>umbral3)
  agrupa.rets3<-menor.umbr3
  as.ts(agrupa.rets3)
  par(mfrow=c(1,1))
  plot(agrupa.rets3)
}

umb3(.99)
#LOS VALORES SE AGLOMERAN


#h:5 leptocurtosis
par(mfrow=c(1,1))
plot(density(na.omit(ret.n225, main="Densidad de los retornos del N225")))
z3<-seq(-5,5,len=1000)
x3<-dnorm(z3,mean=mean(ret.n225,na.rm=T),sd=sd(ret.n225,na.rm=T))
lines(z3,x3,col='red',lty=2,lwd=2)
#qq plot para determinar fat tails
w3<-na.omit(coredata(ret.n225))
qqnorm(w3)
qqline(w3,col='red',lty=2)
#se observa informacisn a los extremos el cual es factor para tener colas anchas

#h:6 leverage efect
rets<-function(a){
  b<-diff(log(Ad(a)))
  return(b)
}
dato3<-na.omit(ret.n225)
mas3<-apply.daily(dato3,function(a) max(a,0))
menos3<-apply.daily(dato3,function(a) -min(a,0))
abs3<-apply.daily(dato3,function(a) abs(a))
lag2.plot(mas3,abs3,8)
lag2.plot(menos3,abs3,8)
#choques negativos y psitivos son asimetricos, los positivos hacen que los
#retornos varien menos que los negativos
#justificaci??n:??Retornos se modelan mejor con ruido blanco debnil o fuerte?
#Ruido blanco Fuerte
#Ya que su funci??n de autocorrelaci??  de los retornos logaritmicos, #
#no  muestran  significacien sus lags.
#recoredemos que independencia nos indica no correlaci??, en el cual est?? no es el caso. 
#Con el Plot de la serie podemos ver como un valor elevado no sigue el comportamiento de su 
#valor o sus valores pasados, cae abruptamento como si fuera un arch sin tanta memoria.


#EVALUANDO HECHOS ESTILIZADOS PARA ^AEX 
getSymbols("^AEX")
#generando variable
aex<-get("AEX")
#se seleccionan los precios ajustados
aex<- Ad(aex)
#h:1 precios de las acciones impredecibles
par(mfrow=c(2,1))
ret.aex<-diff(log(aex))
plot(ret.aex, main="retornos AEX")
plot(aex, main="AEX")
#h:2 retornos no correlacionado
par(mfrow=c(2,1)) 
acf(na.omit(ret.aex))
pacf(na.omit(ret.aex))
tsdisplay(ret.aex, lag.max = 20)
#h:3 retornos al cuadrado correlacionados
r.aex<-ret.aex^2 
par(mfrow=c(2,1))
acf(na.omit(r.aex))
pacf(na.omit(r.aex))
tsdisplay(na.omit(r.aex))
#h:4 closters de volatilidad
par(mfrow=c(3,1))
plot(na.omit(ret.aex,main="retornos AEX")) 
plot(na.omit(r.aex,main="retornos al cuadrado AEX")) 
plot(na.omit(abs(ret.aex),main="retornos en absolutos AEX")) 
#podemos observar que tanto en los retornos, sus cuadrados y en valores absolutos es notable los closters
par(mfrow=c(2,1))
acf(na.omit(ret.aex,main="retornos AEX"),lag.max = 500) 
acf(na.omit(r.aex,main="retornos al cuadrado AEX"),lag.max = 500)
#podemos observar significancia en lags alejados, se nota volatilidad por percistencia de autocorrelacisn
#la varianza puede estar condicionada a sus valores pasados lo que determina closters
umbral4 <- quantile(na.omit(ret.aex),.95)
menor.umbr4<-as.numeric(ret.aex>umbral4)
agrupa.rets4<-menor.umbr4
as.ts(agrupa.rets4)
par(mfrow=c(1,1))
plot(agrupa.rets4)
#LOS VALORES SE AGLOMERAN

umb4<-function(x){
  umbral4 <- quantile(na.omit(ret.aex),x)
  menor.umbr4<-as.numeric(ret.aex>umbral4)
  agrupa.rets4<-menor.umbr4
  as.ts(agrupa.rets4)
  par(mfrow=c(1,1))
  plot(agrupa.rets4)
}

umb4(.99)
#LOS VALORES SE AGLOMERAN
#h:5 leptocurtosis
par(mfrow=c(1,1))
plot(density(na.omit(ret.aex, main="Densidad de los retornos del AEX")))
z4<-seq(-5,5,len=1000)
x4<-dnorm(z4,mean=mean(ret.aex,na.rm=T),sd=sd(ret.aex,na.rm=T))
lines(z4,x4,col='red',lty=2,lwd=2)
#qq plot para determinar fat tails
w4<-na.omit(coredata(ret.aex))
qqnorm(w4)
qqline(w4,col='red',lty=2)
#se observa informacisn a los extremos el cual es factor para tener colas anchas

#h:6 leverage efect
rets<-function(a){
  b<-diff(log(Ad(a)))
  return(b)
}
dato4<-na.omit(ret.aex)
mas4<-apply.daily(dato4,function(a) max(a,0))
menos4<-apply.daily(dato4,function(a) -min(a,0))
abs4<-apply.daily(dato4,function(a) abs(a))
lag2.plot(mas4,abs4,8)
lag2.plot(menos4,abs4,8)
#choques negativos y psitivos son asimetricos, los positivos hacen que los
#retornos varien menos que los negativos
#justificaci??n:??Retornos se modelan mejor con ruido blanco debnil o fuerte?
#Ruido blanco Debil
#Ya que su funci??n de autocorrelaci??  de los retornos logaritmicos, #
#muestran  significacia en sus lags.
#recoredemos que independencia nos indica no correlaci??, en el cual est?? no es el caso. 
#Con el Plot de la serie podemos ver como un valor elevado sigue el comportamiento de su 
#valor o sus valores pasados.

#EVALUANDO HECHOS ESTILIZADOS PARA MXX
getSymbols("^MXX")
#generando variable
mxx<-get("MXX")
#se seleccionan los precios ajustados
mxx <- Ad(mxx)
#h:1 precios de las acciones impredecibles
par(mfrow=c(2,1))
ret.mxx<-diff(log(mxx))
plot(ret.mxx, main="retornos MXX")
plot(mxx, main="MXX")
#h:2 retornos no correlacionado
par(mfrow=c(2,1))
acf(na.omit(ret.mxx))
pacf(na.omit(ret.mxx))
tsdisplay(ret.mxx, lag.max = 20)
#h:3 retornos al cuadrado correlacionados
par(mfrow=c(2,1))
r.mxx<-ret.mxx^2 
acf(na.omit(r.mxx))
pacf(na.omit(r.mxx))
tsdisplay(na.omit(r.mxx))
#h:4 closters de volatilidad
par(mfrow=c(3,1))
plot(na.omit(ret.mxx,main="retornos MXX")) 
plot(na.omit(r.mxx,main="retornos al cuadrado MXX")) 
plot(na.omit(abs(ret.mxx),main="retornos en absolutos MXX")) 
#podemos observar que tanto en los retornos, sus cuadrados y en valores absolutos es notable los closters
par(mfrow=c(2,1))
acf(na.omit(ret.mxx,main="retornos MXX"),lag.max = 500) 
acf(na.omit(r.mxx,main="retornos al cuadrado MXX"),lag.max = 500)
#podemos observar significancia en lags alejados, se nota volatilidad por percistencia de autocorrelacisn
#la varianza puede estar condicionada a sus valores pasados lo que determina closters
umbral5 <- quantile(na.omit(ret.mxx),.95)
menor.umbr5<-as.numeric(ret.mxx>umbral5)
agrupa.rets5<-menor.umbr5
as.ts(agrupa.rets5)
par(mfrow=c(1,1))
plot(agrupa.rets5)
#LOS VALORES SE AGLOMERAN

umb5<-function(x){
  umbral5 <- quantile(na.omit(ret.mxx),x)
  menor.umbr5<-as.numeric(ret.mxx>umbral5)
  agrupa.rets5<-menor.umbr5
  as.ts(agrupa.rets5)
  par(mfrow=c(1,1))
  plot(agrupa.rets5)
}

umb5(.99)
#h:5 leptocurtosis
par(mfrow=c(1,1))
plot(density(na.omit(ret.mxx, main="Densidad de los retornos del MXX")))
z5<-seq(-5,5,len=1000)
x5<-dnorm(z5,mean=mean(ret.mxx,na.rm=T),sd=sd(ret.mxx,na.rm=T))
lines(z5,x5,col='red',lty=2,lwd=2)
#qq plot para determinar fat tails
w5<-na.omit(coredata(ret.mxx))
qqnorm(w5)
qqline(w5,col='red',lty=2)
#se observa informacisn a los extremos el cual es factor para tener colas anchas

#h:6 leverage efect
rets<-function(a){
  b<-diff(log(Ad(a)))
  return(b)
}
dato5<-na.omit(ret.mxx)
mas5<-apply.daily(dato5,function(a) max(a,0))
menos5<-apply.daily(dato5,function(a) -min(a,0))
abs5<-apply.daily(dato5,function(a) abs(a))
lag2.plot(mas5,abs5,8)
lag2.plot(menos5,abs5,8)
#choques negativos y psitivos son asimetricos, los positivos hacen que los
#retornos varien menos que los negativos
#justificaci??n:??Retornos se modelan mejor con ruido blanco debnil o fuerte?
#Ruido blanco Debil
#Ya que su funci??n de autocorrelaci??  de los retornos logaritmicos, #
#muestran  significacia en sus lags.
#recoredemos que independencia nos indica no correlaci??, en el cual est?? no es el caso. 
#Con el Plot de la serie podemos ver como un valor elevado sigue el comportamiento de su 
#valor o sus valores pasados.
#EVALUANDO HECHOS ESTILIZADOS PARA FCHI
getSymbols("^FCHI")
#generando variable
fchi<-get("FCHI")
#se seleccionan los precios ajustados
fchi <- Ad(fchi)
#h:1 precios de las acciones impredecibles
par(mfrow=c(2,1))
ret.fchi<-diff(log(fchi))
plot(ret.fchi, main="retornos FCHI")
plot(fchi, main="FCHI")
#h:2 retornos no correlacionado
par(mfrow=c(2,1))
acf(na.omit(ret.fchi))
pacf(na.omit(ret.fchi))
tsdisplay(ret.fchi, lag.max = 20)
#h:3 retornos al cuadrado correlacionados
par(mfrow=c(2,1))
r.fchi<-ret.fchi^2 
acf(na.omit(r.fchi))
pacf(na.omit(r.fchi))
tsdisplay(na.omit(r.fchi))
#h:4 closters de volatilidad
par(mfrow=c(3,1))
plot(na.omit(ret.fchi,main="retornos FCHI")) 
plot(na.omit(r.fchi,main="retornos al cuadrado FCHI")) 
plot(na.omit(abs(ret.fchi),main="retornos en absolutos FCHI")) 
#podemos observar que tanto en los retornos, sus cuadrados y en valores absolutos es notable los closters
par(mfrow=c(2,1))
acf(na.omit(ret.fchi,main="retornos FCHI"),lag.max = 500) 
acf(na.omit(r.fchi,main="retornos al cuadrado FCHI"),lag.max = 500)
#podemos observar significancia en lags alejados, se nota volatilidad por percistencia de autocorrelacisn
#la varianza puede estar condicionada a sus valores pasados lo que determina closters
umbral6 <- quantile(na.omit(ret.fchi),.95)
menor.umbr6<-as.numeric(ret.fchi>umbral6)
agrupa.rets6<-menor.umbr6
as.ts(agrupa.rets6)
par(mfrow=c(1,1))
plot(agrupa.rets6)
#LOS VALORES SE AGLOMERAN

umb6<-function(x){
  umbral6 <- quantile(na.omit(ret.fchi),x)
  menor.umbr6<-as.numeric(ret.fchi>umbral6)
  agrupa.rets6<-menor.umbr6
  as.ts(agrupa.rets6)
  par(mfrow=c(1,1))
  plot(agrupa.rets6)
}

umb6(.99)
#h:5 leptocurtosis
par(mfrow=c(1,1))
plot(density(na.omit(ret.fchi, main="Densidad de los retornos del FCHI")))
z6<-seq(-5,5,len=1000)
x6<-dnorm(z6,mean=mean(ret.fchi,na.rm=T),sd=sd(ret.fchi,na.rm=T))
lines(z6,x6,col='red',lty=2,lwd=2)
#qq plot para determinar fat tails
w6<-na.omit(coredata(ret.fchi))
qqnorm(w6)
qqline(w6,col='red',lty=2)
#se observa informacisn a los extremos el cual es factor para tener colas anchas

#h:6 leverage efect
rets<-function(a){
  b<-diff(log(Ad(a)))
  return(b)
}
dato6<-na.omit(ret.fchi)
mas6<-apply.daily(dato6,function(a) max(a,0))
menos6<-apply.daily(dato6,function(a) -min(a,0))
abs6<-apply.daily(dato6,function(a) abs(a))
lag2.plot(mas6,abs6,8)
lag2.plot(menos6,abs6,8)
#choques negativos y psitivos son asimetricos, los positivos hacen que los
#retornos varien menos que los negativos
#justificaci??n:??Retornos se modelan mejor con ruido blanco debnil o fuerte?
#Ruido blanco Debil
#Ya que su funci??n de autocorrelaci??  de los retornos logaritmicos, #
#muestran  significacia en sus lags.
#recoredemos que independencia nos indica no correlaci??, en el cual est?? no es el caso. 
#Con el Plot de la serie podemos ver como un valor elevado sigue el comportamiento de su 
#valor o sus valores pasados.

#############FIN EJERCICIO 1#############################################