###############Estacionariedad######################
###############EJERCICIO 4#######################

################# 1 ####################

simula <- function(n.sim){
  alpha <- rnorm(n.sim)
  a<- log( alpha ^ 2 )
  mu <- cumsum(a) / seq_along(a)
  
  err.ind <- a - mu
  err.total <- sum(err.ind^2) 
  sigma.n <- sqrt(err.total) / seq_along(a)
  
  upper <- mu + 1.96*sigma.n
  lower <- mu - 1.96*sigma.n
  
  plot(mu, type = 'l', col = "blue")
  lines(upper, col = "red")
  lines(lower, col = "red")
}
simula(10000)


simula

#Estacionariedad

#######################ejercicio2######################
z<-rnorm(10^4)
estim<-mean(2*z^2+.9)
estim< 1

decide<-function(n.sim,alpha,beta){
  z<-rnorm(n.sim)
  estim<-mean(alpha*z^2+beta)
  estim< 1
}

decide(1000,.2,.3)

###################FIN ejercicio2####################
##############CAMBIANDO VALORES Y ORDENES DEL GARCH####################
#SE PROGRAMA FUNCI??N EN EL CUAL SOLO SE DEBERA CAMBIAR EL ORDEN Y 
#VALORES DEL GARCH Y SUS PARAMETROS########
alpha=c(2,1)
beta=c(0)

garch_matrix<-function(alpha,beta){
  renglones <- rbind(c(alpha*rnorm(1)^2, beta*rnorm(1)^2), #p rimer rengl??n 
                     cbind(diag(1,length(alpha)-1,length(beta)),
                           diag(0,length(alpha)-1,length(alpha))),
                     c(alpha, beta), cbind(diag(0,length(beta)-1,length(alpha)),
                                           diag(1,length(beta)-1,length(beta))))
  A <- matrix(renglones, nrow = length(alpha)+length(beta))
}

garch_matrix(alpha,beta)
library(purrr)

matrices <- rerun(500, garch_matrix(alpha, beta))
prods <- accumulate(matrices, `%*%`) 
normas <- map_dbl(prods, norm)
lyapunov <- log(normas) / seq_along(normas)
#valor de .2936771
plot(lyapunov, type = "l")

#########FIN DEL 3#######
