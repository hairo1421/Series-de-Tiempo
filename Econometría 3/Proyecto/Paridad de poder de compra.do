 clear
 clear matrix

 log using "C:\Users\HairoUlises\Desktop\PPC.smcl"
 
 import excel "C:\Users\HairoUlises\Documents\Economía\5to Semestre\Econometría III\PPP_Trabajo_Final\database.xlsx", sheet("Hoja1") first


*Estableciendo la variable fecha 
set obs 638
generate date=tm(1963m1)+_n-1
format %tm date
list date in 1
tsset date
drop Year

*** Análisis Exploratorio de las series
tsline ipc_mx
tsline ipc_us ipc_can
tsline tc_pd tc_pcd
tsline tc_pd tc_pcd tc_dcd
tsline tc_dcd 


*** Modificando variables
tssmooth ma sipc_mx = ipc_mx , window(3 1 2) 
tssmooth ma sipc_us = ipc_us , window(3 1 2) 
tssmooth ma sipc_can = ipc_can , window(3 1 2) 
tssmooth ma stc_pd = tc_pd , window(3 1 2) 
tssmooth ma stc_pcd = tc_pcd , window(3 1 2) 
tssmooth ma stc_dcd = tc_dcd , window(3 1 2) 

* Comparando las series originales con las desestacionalizadas
tsline sipc_mx ipc_mx
tsline sipc_us ipc_us
tsline sipc_can ipc_can
tsline stc_pd tc_pd
tsline stc_pcd tc_pcd
tsline stc_dcd tc_dcd

* PASO 1, GENERAR TODAS LAS SERIES QUE SE VAN A UTILIZAR

*Generando y viendo los logaritmos de las series
gen lipc_mx=log(sipc_mx)
gen lipc_us=log(sipc_us)
gen lipc_can=log(sipc_can)
gen ltc_pd=log(stc_pd)
gen ltc_pcd=log(stc_pcd)
gen ltc_dcd=log(stc_pcd)

tsline lipc_mx
tsline lipc_us lipc_can
tsline lipc_us lipc_can lipc_mx
tsline ltc_pd ltc_pcd



**** Calculando tipos de camibio reales ****
** Para Mex-USA
gen tcr_us=lipc_mx-lipc_us-ltc_pd
tsline tcr_us

** Para Mex-Canada
gen tcr_can=lipc_mx-lipc_can-ltc_pcd
tsline  tcr_can



****** PASO 2, PRUEBAS DE RAIZ UNITARIA AL TIPO DE CAMBIO REAL

**** Para el tipo de cambio real Mex/EEUU ****

***Primera aproximación
tsline tcr_us
tsline d.tcr_us
* La serie muestra cambios estructurales

zandrews tcr_us, break(both) lagmethod(AIC) trim(0.05) graph
zandrews tcr_us, lagmethod(AIC) trim(0.05) graph
* Controlando los cambios Estructurales se acepta la H0, por lo tanto, la 
* serie No es itegrada de orden 0


**** Para el tipo de cambio real Mex/Canada ****

tsline tcr_can
tsline d.tcr_can
* La serie muestra cambios estructurales

zandrews tcr_can, lagmethod(AIC) trim(0.05) graph
zandrews tcr_can, break(both) lagmethod(AIC) trim(0.05) graph
* Controlando los cambios estructurales se acepta la H0, por lo que se concluye
* que la serie tiene Raíz Unitaria

 *********************************************************************
 *****Se concluye que el tipo de cambio real mex-us y mx-can**********
 *****no son estacionarios  detectando cambio estructural en ambas****
 ******por lo tanto no se apoya la hipótesis de PPC*********
 *********************************************************************

 
 
 
*********** PASO 3, COINTEGRACION (engel & grengel)***********


********* PASO 3.1, Se busca el Orden de Integración de las Variables **

*****************************************************************
*****************************************************************
**** Para la variable lipc_mx
tsline lipc_mx
tsline d.lipc_mx
tsline d2.lipc_mx

*Dado a que se ve cambio estructural en la tendencia se utiliza:
 zandrews lipc_mx, maxlags(12) trim(0.05) lagmethod(AIC) break(trend) graph
*se acepta Ho (proceso de raíz unitaria) al 1%, 5% & 10%
*LIPC_MX no es I~(0)

 ******primera diferencia ipc_mx**
*el grafico muestra posibles cambios de niveles:
*se rechaza Ho (proceso de raíz unitaria) al 1%
zandrews d.lipc_mx, maxlags(12) trim(0.05) break(both) lagmethod(AIC)  graph
*DIPC_MX I~(0)

*IPC_MX I~(1)

*****************************************************************
*****************************************************************
**** Para la variable lipc_us
tsline lipc_us
tsline d.lipc_us

* En ambas se acepta H0 (proceso de raíz unitaria)
dfuller d.lipc_us, trend regress lags(8) 
*LIPC_US no es I~(0)


 ******Primera diferencia ipc_us**
*se rechaza Ho (proceso de raíz unitaria) al 1%
dfuller d.lipc_us, regress lags(2)
 *DIPC_US I~(0)

 *IPC_US I~(1)
*****************************************************************
*****************************************************************
**** Para la variable lipc_can
 tsline lipc_can
 tsline d.lipc_can
 
 *se acepta Ho (proceso de raíz unitaria) al 1%, 5% & 10%
 dfuller lipc_can, lags(3) tr  reg
*LIPC_CAN no es I~(0)

 ******Primera diferencia ipc_can**
*se rechaza Ho (proceso de raíz unitaria) al 1%
 dfuller d.lipc_can, lags(3)   reg
 *DIPC_CAN I~(0) 
 
*IPC_CAN I~(1) 
*****************************************************************
*****************************************************************
**** Para la variable ltc_pd

tsline ltc_pd
tsline d.ltc_pd

*se ve con cambio estructuran en tendencia y en niveles:
*se rechaza Ho(proceso de raíz unitaria) al 1%
zandrews ltc_pd, break(both) trim(0.05) graph
*LTC_PD es I~(0)

******Primera diferencia ipc_can**
*se rechaza Ho (proceso de raíz unitaria) al 1%
zandrews d.ltc_pd, maxlags(12) trim(0.05) lagmethod(AIC)  graph
*DTC_PDC I~(0)

*LTC_PD ES I~(0)

*****************************************************************
*****************************************************************
**** Para la variable ltc_pcd

tsline ltc_pcd
tsline d.ltc_pcd

*se ve con cambio estructuran en tendencia y en niveles:
*se rechaza Ho (proceso de raíz unitaria) al 1%
 zandrews ltc_pcd, maxlags(12) trim(0.05) lagmethod(AIC) break(both) graph
*LTC_PDC es I~(0)

******Primera diferencia ipc_can**
*se rechaza Ho (proceso de raíz unitaria) al 1%
zandrews d.ltc_pcd, maxlags(12) trim(0.05) lagmethod(AIC)  graph
*DTC_PDC I~(0)

*LTC_PDC ES I~(0)

*************************************************************************
*************************************************************************
*IPC_CAN I~(1)
*IPC_US I~(1)
*IPC_CAN I~(1) 
*LTC_PD ES I~(0)
*LTC_PDC ES I~(0)



********* PASO 3.2, Sacar la relación de largo plazo

*** Se Saca la Relación de largo plazo con las Variables Originales ***

***************************************************
*** Para Mex-EU
***************************************************

*Ecuación 1
reg lipc_mx ltc_pd lipc_us
predict res_us1, residuals
tsline res_us1

*Ecuación 2
reg lipc_us ltc_pd lipc_mx
predict res_us2, residuals
tsline res_us2

*Ecuación 3
reg ltc_pd lipc_mx lipc_us
predict res_us3, residuals
tsline res_us3

tsline res_us1 res_us2 res_us3

* Pruebas a los Residuales

* Ecuación 1
zandrews res_us1, maxlags(12) trim(.05) lagmethod(AIC) graph
* Se acepta H0 (proceso de raíz unitaria) 

* Ecuación 2
zandrews res_us2, maxlags(12) trim(.05) lagmethod(AIC) graph
* Se acepta H0 (proceso de raíz unitaria) 

* Ecuación 3
zandrews res_us3, maxlags(12) trim(.05) lagmethod(AIC) graph
* Se acepta H0 (proceso de raíz unitaria) 

***************************************************
*** Para Mex-Can
***************************************************

*Ecuación 1
reg lipc_mx ltc_pcd lipc_can
predict res_can1, residuals
tsline res_can1

*Ecuación 2
reg lipc_can ltc_pcd lipc_mx
predict res_can2, residuals
tsline res_can2

*Ecuación 3
reg ltc_pcd lipc_mx lipc_can
predict res_can3, residuals
tsline res_can3

tsline res_can1 res_can2 res_can3


* Pruebas a los Residuales

* Ecuación 1
dfuller res_can1, noconstant regress lags(3)
zandrews res_can1, maxlags(12) trim(.05) lagmethod(AIC) graph
* Se acepta H0 (proceso de raíz unitaria) 

* Ecuación 2
dfuller res_can2, noconstant regress lags(8)
zandrews res_can2, maxlags(12) trim(.05) lagmethod(AIC) graph
* Se acepta H0 (proceso de raíz unitaria) 

* Ecuación 3
dfuller res_can3, noconstant regress lags(4)
zandrews res_can3, maxlags(12) trim(.05) lagmethod(AIC) graph
* Se acepta H0 (proceso de raíz unitaria) 



***************************************************
***************************************************
*********Modelo de Corrección de Errores***********
***************************************************
***************************************************
***************************************************


***************************************************
*** Para Mex-EU
***************************************************
* Ecuación 1
reg d.lipc_mx l.d.lipc_mx l2.d.lipc_mx l3.d.lipc_mx l4.d.lipc_mx  l5.d.lipc_mx l6.d.lipc_mx l7.d.lipc_mx l8.d.lipc_mx   l.d.lipc_us l2.d.lipc_us l3.d.lipc_us l4.d.lipc_us l5.d.lipc_us l6.d.lipc_us  l7.d.lipc_us l8.d.lipc_us   l.d.ltc_pd l2.d.ltc_pd l3.d.ltc_pd  l4.d.ltc_pd l5.d.ltc_pd l6.d.ltc_pd l7.d.ltc_pd l8.d.ltc_pd  res_us1
*Más rezagos para normalidad del error
reg d.lipc_mx l.d.lipc_mx l2.d.lipc_mx l3.d.lipc_mx l4.d.lipc_mx  l5.d.lipc_mx l6.d.lipc_mx l7.d.lipc_mx l8.d.lipc_mx l9.d.lipc_mx  l10.d.lipc_mx l11.d.lipc_mx  l.d.lipc_us l2.d.lipc_us l3.d.lipc_us l4.d.lipc_us l5.d.lipc_us l6.d.lipc_us  l7.d.lipc_us l8.d.lipc_us l9.d.lipc_us l10.d.lipc_us l11.d.lipc_us  l.d.ltc_pd l2.d.ltc_pd l3.d.ltc_pd  l4.d.ltc_pd l5.d.ltc_pd l6.d.ltc_pd l7.d.ltc_pd l8.d.ltc_pd  l9.d.ltc_pd l10.d.ltc_pd l11.d.ltc_pd  res_us1
*MODELO SELECCIONADO
reg d.lipc_mx l.d.lipc_mx l2.d.lipc_mx    l6.d.lipc_mx l7.d.lipc_mx l8.d.lipc_mx    l3.d.lipc_us    l.d.ltc_pd   l4.d.ltc_pd l5.d.ltc_pd  l8.d.ltc_pd  res_us1, nocons
predict err_eq1us, r
tsline err_eq1us
jb err_eq1us 
histogram err_eq1us
histogram err_eq1us,norm
*no normal

* Ecuación 2
reg d.lipc_us l.d.lipc_mx l2.d.lipc_mx l3.d.lipc_mx l.d.lipc_us l2.d.lipc_us l3.d.lipc_us l.d.ltc_pd l2.d.ltc_pd l3.d.ltc_pd res_us2
*Más rezagos para normalidad del error
reg d.lipc_us l.d.lipc_mx l2.d.lipc_mx l3.d.lipc_mx l4.d.lipc_mx  l5.d.lipc_mx l6.d.lipc_mx l7.d.lipc_mx l8.d.lipc_mx   l.d.lipc_us l2.d.lipc_us l3.d.lipc_us l4.d.lipc_us l5.d.lipc_us l6.d.lipc_us  l7.d.lipc_us l8.d.lipc_us   l.d.ltc_pd l2.d.ltc_pd l3.d.ltc_pd  l4.d.ltc_pd l5.d.ltc_pd l6.d.ltc_pd l7.d.ltc_pd l8.d.ltc_pd  res_us1
*MODELO SELECCIONADO
reg d.lipc_us  l.d.lipc_us l2.d.lipc_us   l2.d.ltc_pd l3.d.ltc_pd res_us2
predict err_eq2us, r
tsline err_eq2us
jb err_eq2us 
histogram err_eq2us
histogram err_eq2us,norm
*no normal

* Ecuación 3
reg d.ltc_pd l.d.lipc_mx l2.d.lipc_mx l3.d.lipc_mx l.d.lipc_us l2.d.lipc_us l3.d.lipc_us l.d.ltc_pd l2.d.ltc_pd l3.d.ltc_pd res_us3
*Más rezagos para normalidad del error
reg d.ltc_pd l.d.lipc_mx l2.d.lipc_mx l3.d.lipc_mx l4.d.lipc_mx  l5.d.lipc_mx l6.d.lipc_mx l7.d.lipc_mx l8.d.lipc_mx   l.d.lipc_us l2.d.lipc_us l3.d.lipc_us l4.d.lipc_us l5.d.lipc_us l6.d.lipc_us  l7.d.lipc_us l8.d.lipc_us   l.d.ltc_pd l2.d.ltc_pd l3.d.ltc_pd  l4.d.ltc_pd l5.d.ltc_pd l6.d.ltc_pd l7.d.ltc_pd l8.d.ltc_pd  res_us1
*MODELO SELECCIONADO
reg d.ltc_pd l.d.lipc_mx l2.d.lipc_mx l3.d.lipc_mx  l3.d.lipc_us l.d.ltc_pd  res_us3, nocons
predict err_eq3us, r
tsline err_eq3us
jb err_eq3us 
histogram err_eq3us
histogram err_eq3us,norm
*no normal


***************************************************
*** Para Mex-Can
***************************************************
* Ecuación 1
reg d.lipc_mx l.d.lipc_mx l2.d.lipc_mx l3.d.lipc_mx l.d.lipc_can l2.d.lipc_can l3.d.lipc_can l.d.ltc_pcd l2.d.ltc_pcd l3.d.ltc_pcd res_can1
*Más rezagos para normalidad del error
reg d.lipc_mx l.d.lipc_mx l2.d.lipc_mx l3.d.lipc_mx l4.d.lipc_mx  l5.d.lipc_mx l6.d.lipc_mx l7.d.lipc_mx l8.d.lipc_mx l9.d.lipc_mx l10.d.lipc_mx l11.d.lipc_mx l12.d.lipc_mx l13.d.lipc_mx l14.d.lipc_mx l15.d.lipc_mx l16.d.lipc_mx   l.d.lipc_can l2.d.lipc_can l3.d.lipc_can l4.d.lipc_can l5.d.lipc_can l6.d.lipc_can  l7.d.lipc_can l8.d.lipc_can l9.d.lipc_can l10.d.lipc_can l11.d.lipc_can l12.d.lipc_can  l13.d.lipc_can l14.d.lipc_can l15.d.lipc_can  l16.d.lipc_can l.d.ltc_pcd l2.d.ltc_pcd l3.d.ltc_pcd  l4.d.ltc_pcd l5.d.ltc_pcd l6.d.ltc_pcd l7.d.ltc_pcd l8.d.ltc_pcd l9.d.ltc_pcd l10.d.ltc_pcd l11.d.ltc_pcd l12.d.ltc_pcd l13.d.ltc_pcd l14.d.ltc_pcd l15.d.ltc_pcd l16.d.ltc_pcd   res_can1
*MODELO SELECCIONADO
reg d.lipc_mx l.d.lipc_mx l2.d.lipc_mx  l.d.ltc_pcd l2.d.ltc_pcd l3.d.ltc_pcd res_can1,nocons
predict err_eq1can, r
tsline err_eq1can
jb err_eq1can 
histogram err_eq1can
histogram err_eq1can,norm
*no normal


* Ecuación 2
reg d.lipc_can l.d.lipc_mx l2.d.lipc_mx l3.d.lipc_mx l.d.lipc_can l2.d.lipc_can l3.d.lipc_can l.d.ltc_pcd l2.d.ltc_pcd l3.d.ltc_pcd res_can2
*Más rezagos para normalidad del error
reg d.lipc_can l.d.lipc_mx l2.d.lipc_mx l3.d.lipc_mx l4.d.lipc_mx  l5.d.lipc_mx l6.d.lipc_mx l7.d.lipc_mx l8.d.lipc_mx l9.d.lipc_mx l10.d.lipc_mx l11.d.lipc_mx l12.d.lipc_mx l13.d.lipc_mx l14.d.lipc_mx l15.d.lipc_mx l16.d.lipc_mx   l.d.lipc_can l2.d.lipc_can l3.d.lipc_can l4.d.lipc_can l5.d.lipc_can l6.d.lipc_can  l7.d.lipc_can l8.d.lipc_can l9.d.lipc_can l10.d.lipc_can l11.d.lipc_can l12.d.lipc_can  l13.d.lipc_can l14.d.lipc_can l15.d.lipc_can  l16.d.lipc_can l.d.ltc_pcd l2.d.ltc_pcd l3.d.ltc_pcd  l4.d.ltc_pcd l5.d.ltc_pcd l6.d.ltc_pcd l7.d.ltc_pcd l8.d.ltc_pcd l9.d.ltc_pcd l10.d.ltc_pcd l11.d.ltc_pcd l12.d.ltc_pcd l13.d.ltc_pcd l14.d.ltc_pcd l15.d.ltc_pcd l16.d.ltc_pcd   res_can2
*MODELO SELECCIONADO
reg d.lipc_can l2.d.lipc_mx l3.d.lipc_mx l.d.lipc_can l2.d.lipc_can   res_can2
predict err_eq2can, r
tsline err_eq2can
jb err_eq2can 
histogram err_eq2can
histogram err_eq2can,norm
*no normal

* Ecuación 3
reg d.ltc_pcd l.d.lipc_mx l2.d.lipc_mx l3.d.lipc_mx l.d.lipc_can l2.d.lipc_can l3.d.lipc_can l.d.ltc_pcd l2.d.ltc_pcd l3.d.ltc_pcd res_can3
*Más rezagos para normalidad del error
reg d.ltc_pcd l.d.lipc_mx l2.d.lipc_mx l3.d.lipc_mx l4.d.lipc_mx  l5.d.lipc_mx l6.d.lipc_mx l7.d.lipc_mx l8.d.lipc_mx l9.d.lipc_mx l10.d.lipc_mx l11.d.lipc_mx l12.d.lipc_mx l13.d.lipc_mx l14.d.lipc_mx l15.d.lipc_mx l16.d.lipc_mx   l.d.lipc_can l2.d.lipc_can l3.d.lipc_can l4.d.lipc_can l5.d.lipc_can l6.d.lipc_can  l7.d.lipc_can l8.d.lipc_can l9.d.lipc_can l10.d.lipc_can l11.d.lipc_can l12.d.lipc_can  l13.d.lipc_can l14.d.lipc_can l15.d.lipc_can  l16.d.lipc_can l.d.ltc_pcd l2.d.ltc_pcd l3.d.ltc_pcd  l4.d.ltc_pcd l5.d.ltc_pcd l6.d.ltc_pcd l7.d.ltc_pcd l8.d.ltc_pcd l9.d.ltc_pcd l10.d.ltc_pcd l11.d.ltc_pcd l12.d.ltc_pcd l13.d.ltc_pcd l14.d.ltc_pcd l15.d.ltc_pcd l16.d.ltc_pcd   res_can3
*MODELO SELECCIONADO
reg d.ltc_pcd l.d.lipc_mx l2.d.lipc_mx l3.d.lipc_mx  l3.d.lipc_can  l3.d.ltc_pcd res_can3,nocons


predict err_eq3can, r
tsline err_eq3can
jb err_eq3can 
histogram err_eq3can
histogram err_eq3can,norm
*no normal



 **********************************************************************
 *****Se concluye que la prueba a los residuos (ipc_mx ipc_us tc_pd)***
 ***************son estacionarios************************************** 
 ******por lo tanto  se apoya la hipótesis de PPC para MX-US***********
 **********************************************************************
******************ENGEL & GRENGEL APOYA A LA PPC***********************
 **********************************************************************
 *****Se concluye que la prueba a los residuos (ipc_mx ipc_can tc_pcd)*
 ***************son estacionarios**************************************  
 ******por lo tanto  se apoya la hipótesis de PPC para MX-CAN**********
 **********************************************************************

 
*********************************************************************
***** Cointegración Johansen *****
*********************************************************************

**** Para Mex-US

* Buscando el numero de rezagos

* la mayoría de las pruebas indica que el numero de rezagos óptimo es de 4
varsoc lipc_mx ltc_pd lipc_us
*LR-4LAGS
*FPE-4LAGS
*AIC-4LAGS
*HQIC-4LAGS
*BIC-3LAGS


vecrank lipc_mx ltc_pd lipc_us, lags(4) max ic levela
*Ho acepto-Ha rechazo
*Considerando max statistic y trace statistic el labda max y trace indica
*al 99% no existe pero al 95% existe 1 ecuación de cointegración

**********MODELO DE CORRECCION DE ERRORES********

*Modelo con 4  lags
*Modelo con 1 ecuación de cointegración

vec lipc_mx ltc_pd lipc_us, r(1) lags(4)
vec lipc_us ltc_pd lipc_mx, r(1) lags(4)

*El modelo que conservamos para 4 lags
vec ltc_pd lipc_us lipc_mx, r(1) lags(4)


veclmar, mlag(4)
*HO no autocorrelacion se busca aceptar
*no se encuentra evidencia de autocorrelación serial en los rezagos   2 y 3 al 95%
*si autocorrelación con el rezago 1 y 4 al 95%

*Normalidad de los errores modelo VECM
*Ho:Normalidad
*se busca aceptar

vecnorm
* NO NORMAL

************************************************************************
**** Para Mex-Can
************************************************************************
 *Rezagos Optimos

varsoc lipc_mx lipc_can ltc_pcd
*LR-4LAGS
*FPE-4LAGS
*AIC-4LAGS
*HQIC-4LAGS
*BIC-3LAGS 

vecrank lipc_mx lipc_can ltc_pcd, lags(4) max ic levela
*Ho acepto-Ha rechazo
*Considerando max statistic y trace statistic el labda max y trace indica
*al 99% no existe pero al 95% existe 1 ecuación de cointegración

**********MODELO DE CORRECCION DE ERRORES********

*Modelo con 4  lags
*Modelo con 1 ecuación de cointegración
*Modelo usando SBIC 1 ecuacion de cointegración

vec lipc_mx lipc_can ltc_pcd, r(1) lags(4)
vec  lipc_can ltc_pcd lipc_mx, r(1) lags(4)

*cambiando el orden de las variables
vec   ltc_pcd lipc_mx lipc_can, r(1) lags(4)
*El modelo que conservamos para 4 lags

* AUTOCORRELACION DEL RESIDUAL DEL MODELO 
veclmar, mlag(4)
*HO no autocorrelacion se busca aceptar
*no se encuentra evidencia de autocorrelación serial en los rezagos  1, 2 y 3 al 95%
*si autocorrelación con el rezago 4 al 95%

*Normalidad de los errores modelo VECM
*Ho:Normalidad
*se busca aceptar

vecnorm 
**no normales***

****************************************************
****************************************************
*Modelo con 3  lags
*Modelo con 1 ecuación de cointegración
*Modelo usando SBIC 1 ecuacion de cointegración

vec lipc_mx lipc_can ltc_pcd, r(1) lags(3)

*El modelo que conservamos para 3 lags
* AUTOCORRELACION DEL RESIDUAL DEL MODELO 
veclmar, mlag(3)
*HO no autocorrelacion se busca aceptar
*no se encuentra evidencia de autocorrelación serial en los rezagos  1, 2 y 3 al 95%
*si autocorrelación con el rezago 1 y 2 al 95%

*Normalidad de los errores modelo VECM
*Ho:Normalidad
*se busca aceptar

vecnorm 
**no normales***

*****VEC MODELO DE CORRECCIÓN DE ERRO QUE SE ELIGE**
vec   ltc_pcd lipc_mx lipc_can, r(1) lags(4) tr(rc)
*tener en cuenta que no es normal el error.

 **********************************************************************
 *****Se concluye JOHANSEN (ipc_mx ipc_us tc_pd)***
 ***************son estacionarios (1 ECUACIÓN DE COINTEGRACIÓN)******** 
 ******por lo tanto  se apoya la hipótesis de PPC para MX-US***********
 **********************************************************************

  **********************************************************************
 *****Se concluye JOHANSEN (ipc_mx ipc_can tc_pdc)***
 ***************son estacionarios (1 ECUACIÓN DE COINTEGRACIÓN)******** 
 ******por lo tanto  se apoya la hipótesis de PPC para MX-CAN***********
 **********************************************************************

****conclusione: 
*Tipode cambio real mx-can estacionario-PPC no valido, 
*Tipo de cambio real mx-us estacionario-PPC no valido,
*Engel & Grengel ipc_can-ipc_mx-tc_pcd residuales estacionarios-PPC valido
*Engel & Grengel ipc_us-ipc_mx-tc_pd residuales estacionarios-PPC valido
*Johansen ipc_can-ipc_mx-tc_pcd una ecuación de cointegración-PPC valido
*Johansen ipc_us-ipc_mx-tc_pd una ecuación de cointegración-PPC valido

