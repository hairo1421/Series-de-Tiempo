insheet using "C:\Users\HairoUlises\Desktop\5to Semestre\Econometría III\Indices ARIMA\Ecuador\kandp.txt"
set obs 150
generate year2=tm(2003m6)+_n-1
format %tm year2
list year2 in 1
tsset year2
*VARIABLES
*IAE-INDICE DE ACTIVIDAD ECONOMICA (PIB)
*IPC-INDICE DE PRECIOS AL CONSUMIDOR
*TINTN-TASA DE INTERES
*M1-EN MILLONES DE DOLARES
*GASTO-GASTO DEL GOBIRNO (INVERSION) EN MILLONES
*ATCAM-TIPODECAMBIO DOLAR-EURO
*IVPI-INDICE DE VOLUMEN DE PRODUCCION INDUSTRIAL
gen liae=log(iae)
gen lipc=log(ipc)
gen ltintn=log(tintn)
gen lm1=log(m1)
gen lgasto=log(gasto)
gen latcam=log(atcam)
gen livpi=log(ivpi)
*normalidad de la serie
jb iae
*filtros hp
tsfilter hp liaehp=liae
tsfilter hp lipchp=lipc
tsfilter hp ltinthp=ltint
tsfilter hp lm1hp=lm1
tsfilter hp lgastohp=lgasto
tsfilter hp latcamhp=latcam
tsfilter hp livpihp=livpi
*graficas
tsline liae lipc ltintn lm1 lgasto latcam livpi
tsline liae
tsline lipc
tsline ltintn
tsline lm1
tsline lgasto
tsline latcam
tsline livpi
*graficos ciclos
tsline liaehp
tsline lipchp
tsline ltinthp
tsline lm1hp
tsline lgastohp
tsline latcamhp
tsline livpihp
*tendencia 
gen liae_tend=liae-liaehp
gen lipc_tend=lipc-lipchp
gen ltintn_tend=ltintn-ltinthp
gen lm1_tend=lm1-lm1hp
gen lgasto_tend=lgasto-lgastohp
gen latcam_tend=latcam-latcamhp
gen livpi_tend=livpi-livpihp
*grafica tendencia
tsline liae liae_tend
tsline lipc lipc_tend
tsline ltintn ltintn_tend
tsline lm1 lm1_tend
tsline lgasto lgasto_tend
tsline latcam latcam_tend
tsline livpi livpi_tend


*filtros bk
tsfilter bk liaebk=liae
tsfilter bk lipcbk=lipc
tsfilter bk ltintbk=ltintn
tsfilter bk lm1bk=lm1
tsfilter bk lgastobk=lgasto
tsfilter bk latcambk=latcam
tsfilter bk livpibk=livpi
*graficos ciclos
tsline liaebk
tsline lipcbk
tsline ltintbk
tsline lm1bk
tsline lgastobk
tsline latcambk
tsline livpibk
*tendencia 
gen liae_tendbk=liae-liaebk
gen lipc_tendbk=lipc-lipcbk
gen ltintn_tendbk=ltintn-ltintbk
gen lm1_tendbk=lm1-lm1bk
gen lgasto_tendbk=lgasto-lgastobk
gen latcam_tendbk=latcam-latcambk
gen livpi_tendbk=livpi-livpibk
*grafica tendencia
tsline liae liae_tendbk
tsline lipc lipc_tendbk
tsline ltintn ltintn_tendbk
tsline lm1 lm1_tendbk
tsline lgasto lgasto_tendbk
tsline latcam latcam_tendbk
tsline livpi livpi_tendbk
*hp vs bk
tsline liaehp liaebk
tsline lipchp lipcbk
tsline ltinthp ltintbk
tsline lm1hp lm1bk
tsline lgastohp lgastobk
tsline latcamhp latcambk
tsline livpihp livpibk
*correlación ciclos
corr liaehp lipchp ltinthp lm1hp lgastohp latcamhp livpihp
xcorr liaehp lipchp, tab

