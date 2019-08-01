#################Momentos superiores################# 
##############EJERCICIO4##################################

#1###############################
#nota mu4 es una variable aletoria y su cuarto momento es 3
mom.cuatro<-function(mu4,alpha,beta,w) {
  estim<-mu4*((w^2*(1-alpha+beta))/((1-alpha-beta)*(1-mu4)*alpha^2-beta^2-2*alpha*beta))
  
  estim<-alpha+beta
  mu4>0 & estim!=1 & w!=0
  
}

mom.cuatro(3,.2,.8,2)

#EL metodo montecarlo te dar??an valores que convergan por lo tanto
#en cuanto m??s elevada sea la simulaci??n 
# los valores convergen podriendo violar las restricci??nes.

#FIN DEL 1##############################

#2#####################################################
library(purrr)
alpha<-c(.1,.3)
beta<-c(.2,.5)

garch_matrix<-function(alpha,beta){
  renglones <- rbind(c(alpha*rnorm(1)^2, beta*rnorm(1)^2), #p rimer rengl??n 
                     cbind(diag(1,length(alpha)-1,length(beta)),
                           diag(0,length(alpha)-1,length(alpha))),
                     c(alpha, beta), cbind(diag(0,length(beta)-1,length(alpha)),
                                           diag(1,length(beta)-1,length(beta))))
  A <- matrix(renglones, nrow = length(alpha)+length(beta))
  A.2 <- kronecker(A,A)
}

matrices <- rerun(500, garch_matrix(alpha, beta))
prods <- accumulate(matrices, `%*%`) 
normas <- map_dbl(prods, norm)
a<- log(normas) / seq_along(normas)
plot(a)

#si llevas al líite la la esperanza del kronecker tiende a cero lo que equivale
#a un rho menor a uno.

rho <- max(abs(eigen(garch_matrix(alpha, beta))$values))

#No es menor a uno el radio espectral dado el modelo.


