help(arima.sim)
library(forecast)
#arima.sim(model, n, rand.gen = rnorm, innov = rand.gen(n, ...),
#n.start = NA, start.innov = rand.gen(n.start, ...),
#...)

#arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
         # 3sd = sqrt(0.1796))

### An ARIMA simulation
#ts.sim <- arima.sim(list(order = c(1,1,0), ar = 0.7), n = 200)
#ts.plot(ts.sim)

##Ejercicio 1: Simulacisn de ARIMA(p,d,q)
#-----1
par(mfrow=c(1,2))
ts.sim<-arima.sim(n=100,model=list(ar=.9))
ts.plot(ts.sim)

ts.sim2<-arima.sim(n=100,model=list(ar=-.9))
ts.plot(ts.sim2)

#nota coeficientes phi:
# ar >- 1 ocila y diverge
# -1 < ar < 0 ocila y converge
# 0 < ar < 1 cae moderadamente y converge
# ar <  1 explota y diverge

#-----2
par(mfrow=c(1,1))
ts.sim<-arima.sim(n=100,model=list(ar=1))
ts.plot(ts.sim)
# coeficiente del AR no es estacionariol.

par(mfrow=c(1,2))
#Simulando un AR con un rnorm
ar.sim1<-function(n.sim,phi){
  x<-rep(0,n.sim)
  w=rnorm(x)
  z=w
  for (t in 2:n.sim) z[t] <- phi*z[t - 1] + w[t]
  ts.plot(z)
}
ar.sim1(1000,1)


#-------3
ar.sim<-function(n.sim){
  ts.sim<-arima.sim(n= n.sim  , model=list(order=c(1,1,1),ar=0,ma=0))
  ts.plot(ts.sim)
}  
ar.sim(1000) 

#-------4
set.seed(12)
ts.sim<-arima.sim(n= 500  , model = list(ar=c(.3, .4) ))
ts.plot(ts.sim)
tsdisplay(ts.sim)

#utilizando un PACF se podrma ver como tiende a dos picos y despues decae 
#caso contrario el ACF tiende a una caida geometrica.
#esto puede concluir o inferir un modelo de un AR(2)

#-------4 haciendo function para un AR2
par(mfrow=c(1,1))
ar.sim<-function(n.sim,phi,phi2){
  ts.sim<-arima.sim(n= n.sim  , model = list(ar=c(phi, phi2) ))
  ts.plot(ts.sim)
}  
ar.sim(1000,.7,.2) 


##Ejercicio 2: Ajustando modelos
y1<-function(n.sim,phi){
x<-rnorm(n.sim)
z<-rnorm(1,0,1)
y <- phi*x+50+z
ts.plot(y)
}
y1(100,.5)

x<-rnorm(100)
z<-rnorm(1,0,1)
y<- .9*x +50 +z
plot(y,type="l")


#1. Cual es la media
#tesrica para el proceso {Yt}? 
#----50 por el ingtercepto

#2. En la forma Yt = 5+0.9Yt-1 +et, cual 
#es el valor que deberma tomar mu?
#-----
mean(y)
#---49.79266 en promedio


#3--
ar1<-arima(y,order=c(1,0,0))

#Un cambio de una unidad del periodo
#pasado , tendra un efecto negativo
#en el valor actual de un .0634 en promedio.
#siendo asm que el pasado del ar 
#tiene efectos negativos sobre valores del 
#periodo actual
#EL coeficiente es de 49.79 en promedio, 
#lo cual nos indica que independiente mente
#el valor promedio actual sera de 49.7930
#en promedio.
#teniendo significancia solamente el el
#intercepto.

#----4
#El modelo elaborado con la funcion arima
#no describe en su totalidad por la variacisn de
#los coeficientes del autorregresivos.

#----5
library(astsa)
dev.off()
help(sarima)
par("mar")
par(mar=c(1,1,1,1))
sa<-sarima(y, 1,0,0)
#explicando el modelos:
#los residuales a la vista se pueden inferiri que no estan
# autocorrelacionados.
sa[1]
#Me parece mas clara la arima, dado a que arroja a primera instancia
#los coeficientes.


##--Ejercicio 3
dir()
ser<-read.csv("GDPC1.csv" ,header=FALSE,skip = 2)
View(ser)
names(ser)[1] <- "YEAR"
names(ser)[2] <- "GDP"

dev.off()
lines(ser,type="l",col="pink")
class(ser$GDP)
plot(log(ser$GDP),type="l",col="red",main="GDP_US",ylab = "log_GDP",xlab = "year")
plot(diff(log(ser$GDP)),type="l",col="red",main="GDP_US",ylab = "log_GDP",xlab = "year")
library(forecast)
tsdisplay(diff(log(ser$GDP)))
ar1<-arima(diff(log(ser$GDP)),order=c(1,0,0))
tsdisplay(ar1$residuals)
#en la grafica de los ACF vemos como las autocorrelaciones
#caen geometricamente,y en los PACF la primera correlacisn
#sale de las bandas y en seguida cae,
#esto infiere en que el modelo puede ser
#explicado por un AR(1).
#Al utilizar acf y pacf para los residuales 
#de nuestro modelos, podemos concluir la normalidad
#de los errores, no detectando autocorrelacisn
par("mar")
par(mar=c(1,1,1,1))
sa1<-sarima(diff(log(ser$GDP)),1,0,0,1,0,0,12)
#En el modelo sarima, se concluye de manera similar
# a los resultados que se obtienen con el modelo arima,
# los coeficientes varian muy poco  y los errores no son autocorrelacionados
#se puede concluir si utilizamos el aic como crmterio
# de eleccisn del modelo
#sar1_aic=-1824.28
#ar1_aic=-1822.28
#el modelo arima al tener un aic mayor 
#es el mejor mejor modelo entre estos dos.


#####---Ejercicio 4
help(cmort)
dev.off()
plot(cmort,type="l")
plot(log(cmort),type="l")
plot(diff(log(cmort)),
     type="l",col="blue",
     main="Cardiovascular Mortality",
     xlab="aqos",ylab="diff_CM")
tsdisplay(diff(log(cmort)))
#Se utilizo un Modelo AR(1), dado a 
#que el ACF decae geometricamente, y el Pacf
#tiene un pico que sale de los intervalos y 
#despues caen, esto quiere decir que esta 
#autoccorrelacionada con un periodo atras.

cmort1<-diff(log(cmort))
ar.cmort<-arima(cmort1,order = c(1,0,0))
tsdisplay(ar.cmort$residuals)
#si se tienen problemas de autocorrelacisn
# en los residuales.
##ajustando a un ar(2)
ar.cmort2<-arima(cmort1,order = c(2,0,0))
tsdisplay(ar.cmort2$residuals)
##azn se tiene problemas de correlacisn con los
#residuales
#ajustandolo a un sarima
dev.off()
par("mar")
par(mar=c(1,1,1,1))
sarima(cmort1,2,0,0,2,0,0,12)
#modelo ajustado , sin errores correlacionados

####serie jj
d.jj<-diff(log(jj))
plot(log(jj),type="l")
tsdisplay(d.jj)

#la serie pareceria que siguiera una caminata
#aleatoria por las funciones de 
#autocorrelacisn, la ACF pareciera
#estar constantes o tener periodos 
#en los cuales brinca(estacionales)
dev.off()
ar_jj<-arima(d.jj,order = c(4,0,0))
tsdisplay(ar_jj$residuals)

par("mar")
par(mar=c(1,1,1,1))
sarima(d.jj,3,0,0,3,0,0,4)

#nota jbox-ho=no autocorrelacisn, busco aceptar
#con pvalue mayor a .05

#EL mejor modelo lo determina el ar(4),
#y con un sarima (3,0,0,3,0,0,4),
#se presenta un pequeqo pico en el periodo 7
#el cual influye a un pequeqo problema con 
#la normalidad de los residuales.
#para el modelo sarima la grafica de residuales
#estandarizados nos muestra un comportamiento
#no determinista, lo cual es lo que bucamos,
#el acf al igual que los residuales estimados
#con el arima nos muestra un pico en el preriodo 7 mmnimo,
#el normal qq plot nos muestra un comportamiento aceotable
#siguiendo la linea de tendencia.
#el Ljuang -box por el contrario se tiene problemas
#en los primeros dos coeficientes estando en el
#a4rea de significancia, lo cual nos indicacorrelacisn.




#####-Ejercicio 5: Una probadita...
dev.off()

###1
plot(prodn, main="mndice de produccisn (1948-1978)",
     xlab="aqo",ylab="mndice",col="blue")
###2
par("mar")
par(mar=c(1,1,1,1))
tsdisplay(log(prodn))

###3
tsdisplay(diff(log(prodn)),lag.max = 36)

####4
ae_p<-arima(diff(log(prodn)),
      order=c(3,0,0))
tsdisplay(ae_p$residuals)

tsdisplay(diff(diff(log(prodn))),lag.max = 36)

#####5
ae_p2<-arima(diff(diff(log(prodn)),
            order=c(3,0,0)))
tsdisplay(ae_p2$residuals)

#####6
dev.off()
par(mar=c(1,1,1,1))
tsdisplay(diff(log(prodn)),lag.max = 11)


ae_p3<-arima(diff(log(prodn)),
                  order=c(0,0,1))
tsdisplay(ae_p3$residuals)
#el modelo arima(0,0,1), simual los acf y pacf de la serie prodn

####-7
prodn1<-diff(log(prodn))
sar2<-sarima(prodn1,0,1,1,3,0,0,12)
#Con los parametros del inciso 5 & 6 
#el standarized residuals nos muestra un comportamientos no tendencia, lo cual
#es bueno para el modelo.
#el Q-Q plot tiente a sus valores iniciales y finales a salir de su tendencia.
#los p-values se encuentran dentro de la banda de significancia, lo cual no es 
#un buen indicador, ya que nos menciona autocorrelacisn en los residuales.

tsdisplay(prodn1)
sar2<-sarima(prodn1,3,0,0,3,1,0,12)
#optarma el modelo sarima(3,0,0,3,1,0,12)
#Dado a cque no presenta tantos problemas en el ACF y el q-q- un comportamiento
#normal, el ppvalue despues del primer periodo, las correlacisnes son insignificativas
#que es lo que se busca.

#####--8
dev.off()
library(astsa)
plot(unemp)
start(unemp)
end(unemp)
frequency(unemp)
d.unemp<-diff(log(unemp))
plot(d.unemp,main="unemployment",col="red")

library(forecast)
tsdisplay(d.unemp)
tsdisplay(d.unemp)
#saltos cada 11 periodos

#modelo que simule la afc & pacf de
#la serie d.unemp
modelo<-arima(d.unemp,order = c(1,0,0))
par(mar=c(1,1,1,1))
par("mar")
tsdisplay(modelo$residuals)

#modelo para los periodes intercilo
#el modeloque nos da esa forma es:
#los picos de estacionalidad.
tsdisplay(d.unemp, lag.max = 11)
#un ar(3)
modelo2<-arima(d.unemp,order = c(1,0,0))
par(mar=c(1,1,1,1))
par("mar")
tsdisplay(modelo2$residuals,lag.max = 11)
sarima(d.unemp,1,0,0,1,1,0,12)
#Se observa un comportamiento no determinista
#en los residuales standarizados,
#un acf autocorrelacionado en el segundo periodo,
#La gafica del normal QQ de la desviacisn
#de los residuales tienden a estar sobre su tendencia.
#pero con el p value del ljuang box 
#tenemos una serie con correlaciones en todos
#sus rezagos.

#cambiando modelo
sarima(d.unemp,3,0,0,3,1,0,12)
#El modelo al analizar los pvalue del Ljuang Box
#nos muestran que los rezagos 3 y 4 son
#los que caen en la banda de significancia, 
#el grafico del afc nos indica solo una corerelacion
#en el rezago 8
#esti serma mejor modelo que el primero.
