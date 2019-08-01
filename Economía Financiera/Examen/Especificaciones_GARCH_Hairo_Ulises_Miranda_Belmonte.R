## Especificaciones GARCH en R
require(rugarch)

## GARCH(1,1)
mod.med <- list(armaOrder = c(0, 0), include.mean = F)
mod.var1 <- list(model = "sGARCH", garchOrder = c(1, 1))
spec1 <- ugarchspec(variance.model = mod.var1, 
                    mean.model = mod.med)

spec1

## GARCH(1,2)
mod.med <- list(armaOrder = c(0, 0), include.mean = F)
mod.var2 <- list(model = "sGARCH", garchOrder = c(1, 2))
spec2 <- ugarchspec(variance.model = mod.var2, 
                    mean.model = mod.med)

spec2

## GARCH(2,1)
mod.med <- list(armaOrder = c(0, 0), include.mean = F)
mod.var3 <- list(model = "sGARCH", garchOrder = c(2, 1))
spec3 <- ugarchspec(variance.model = mod.var3, 
                    mean.model = mod.med)

spec3
spec3@model$fixed.pars

## ARCH(1)
mod.med <- list(armaOrder = c(0, 0), include.mean = F)
mod.var4 <- list(model = "sGARCH", garchOrder = c(1, 0))
spec4 <- ugarchspec(variance.model = mod.var4, 
                    mean.model = mod.med)

spec4

## ARCH(2)
mod.med <- list(armaOrder = c(0, 0), include.mean = F)
mod.var5 <- list(model = "sGARCH", garchOrder = c(2, 0))
spec5 <- ugarchspec(variance.model = mod.var5, 
                    mean.model = mod.med)
spec5


## IGARCH(1,1)
mod.med <- list(armaOrder = c(0, 0), include.mean = F)
mod.var6 <- list(model = "iGARCH", garchOrder = c(1, 1))
spec6 <- ugarchspec(variance.model = mod.var6, 
                    mean.model = mod.med)
spec6







#---------------------------------------------------------------------------------------------------