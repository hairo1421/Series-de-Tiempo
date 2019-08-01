library(quantmod)
#EJERCICIO 5
#ENCONTRAMOS FUNCI??N SIN EMBARGO NO SE ASEGURA DE QUE SEA CORRECTA 
genlattice <- function(X01, u, d, N) {
  X <- c()
  X[1] <- X0
  count <- 2
  
  for (i in 1:N) {
    for (j in 0:i) {
      X[count] <- X0 * u^j * d^(i-j)
      count <- count + 1
    }
  }
  return(X)
}
genlattice(1,2,.5,6)