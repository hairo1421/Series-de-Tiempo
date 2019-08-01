## Simulacisn con las especificaciones GARCH
require(rugarch)
# GARCH(1,1)
par(mfrow=c(2,1))
## Con Cuarto Momento Finito:
prmtrs1 <- list(omega=1, alpha1 = 0.4, beta1 = 0.2)
spec1 <- ugarchspec(variance.model = mod.var1, 
                    mean.model = mod.med, 
                    fixed.pars = prmtrs1)

garch.sim1 <- ugarchpath(spec1, n.sim = 500)
plot(garch.sim1, which = 2)

## Sin Cuarto Momento Finito
prmtrs1 <- list(omega=1, alpha1 = 0.6, beta1 = 0.2)
spec1 <- ugarchspec(variance.model = mod.var1, 
                    mean.model = mod.med, 
                    fixed.pars = prmtrs1)

garch.sim1 <- ugarchpath(spec1, n.sim = 500)
plot(garch.sim1, which = 2)



# GARCH(1,2)
par(mfrow=c(1,1))
prmtrs2 <- list(omega=1, alpha1 = 0.2, beta1 = 0.4, beta2 = 0.2)
spec2 <- ugarchspec(variance.model = mod.var2, 
                    mean.model = mod.med, 
                    fixed.pars = prmtrs2)

garch.sim2 <- ugarchpath(spec2, n.sim = 500)
plot(garch.sim2, which = 2)


# GARCH(2,1)
par(mfrow=c(1,1))
prmtrs3 <- list(omega=1, alpha1 = 0.2, alpha2 = 0.23, beta1 = 0.5)
spec3 <- ugarchspec(variance.model = mod.var3, 
                    mean.model = mod.med, 
                    fixed.pars = prmtrs3)

garch.sim3 <- ugarchpath(spec3, n.sim = 500)
plot(garch.sim3, which = 2)


# ARCH(1)
par(mfrow=c(2,1))
# Con Cuarto Momento Finito
prmtrs4 <- list(omega=1, alpha1 = 0.36)
spec4 <- ugarchspec(variance.model = mod.var4, 
                    mean.model = mod.med, 
                    fixed.pars = prmtrs4)

garch.sim4 <- ugarchpath(spec4, n.sim = 500)
plot(garch.sim4, which = 2)

# Sin Cuarto Momento Finito
prmtrs4 <- list(omega=1, alpha1 = 0.8)
spec4 <- ugarchspec(variance.model = mod.var4, 
                    mean.model = mod.med, 
                    fixed.pars = prmtrs4)

garch.sim4 <- ugarchpath(spec4, n.sim = 500)
plot(garch.sim4, which = 2)


# ARCH(2)*
par(mfrow=c(2,1))
# Con Cuarto Momento Finito
prmtrs5 <- list(omega=1, alpha1 = 0.2, alpha2 = 0.5)
spec5 <- ugarchspec(variance.model = mod.var5, 
                    mean.model = mod.med, 
                    fixed.pars = prmtrs5)

garch.sim5 <- ugarchpath(spec5, n.sim = 500)
plot(garch.sim5, which = 2)

# Sin Cuarto Momento Finito
prmtrs5 <- list(omega=1, alpha1 = 0.4, alpha2 = 0.5)
spec5 <- ugarchspec(variance.model = mod.var5, 
                    mean.model = mod.med, 
                    fixed.pars = prmtrs5)

garch.sim5 <- ugarchpath(spec5, n.sim = 500)
plot(garch.sim5, which = 2)



# IGARCH(1,1)
par(mfrow=c(1,1))
prmtrs6 <- list(omega=1, alpha1 = 0.9)
spec6 <- ugarchspec(variance.model = mod.var6, 
                    mean.model = mod.med, 
                    fixed.pars = prmtrs6)

garch.sim6 <- ugarchpath(spec6, n.sim = 500)
plot(garch.sim6, which = 2)

