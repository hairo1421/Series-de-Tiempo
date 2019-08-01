format %tq YEAR
list YEAR in 1
tsset YEAR

gen lpib=log(pib)
gen lipc=log(ipc)
gen ltintn=log(tintn)
gen lgasto=log(gasto)
gen latcam=log(atcam)
gen lconsum=log(consum)

*normalidad de la serie
jb pib
*filtros hp
tsfilter hp lpibhp=lpib
tsfilter hp lipchp=lipc
tsfilter hp ltinthp=ltint
tsfilter hp lgastohp=lgasto
tsfilter hp latcamhp=latcam
tsfilter hp lconsumhp=lconsum
*graficas
tsline lpib
tsline lipc
tsline ltintn
tsline lgasto
tsline latcam
tsline lconsum
*graficos ciclos
tsline lpibhp
tsline lipchp
tsline ltinthp
tsline lgastohp
tsline latcamhp
tsline lconsumhp
*tendencia 
gen lpib_tend=lpib-lpibhp
gen lipc_tend=lipc-lipchp
gen ltintn_tend=ltintn-ltinthp
gen lgasto_tend=lgasto-lgastohp
gen latcam_tend=latcam-latcamhp
gen lconsum_tend=lconsum-lconsumhp
*grafica tendencia
tsline lpib lpib_tend
tsline lipc lipc_tend
tsline ltintn ltintn_tend
tsline lgasto lgasto_tend
tsline latcam latcam_tend
tsline lconsum lconsum_tend

*filtros bk
tsfilter bk lpibbk=lpib
tsfilter bk lipcbk=lipc
tsfilter bk ltintbk=ltintn
tsfilter bk lgastobk=lgasto
tsfilter bk latcambk=latcam
tsfilter bk lconsumbk=lconsum
*graficos ciclos
tsline lpibbk
tsline lipcbk
tsline ltintbk
tsline lgastobk
tsline latcambk
tsline lconsumbk
*tendencia 
gen lpib_tendbk=lpib-lpibbk
gen lipc_tendbk=lipc-lipcbk
gen ltintn_tendbk=ltintn-ltintbk
gen lgasto_tendbk=lgasto-lgastobk
gen latcam_tendbk=latcam-latcambk
gen lconsum_tendbk=lconsum-lconsumbk
*grafica tendencia BK
tsline lpib lpib_tendbk
tsline lipc lipc_tendbk
tsline ltintn ltintn_tendbk
tsline lgasto lgasto_tendbk
tsline latcam latcam_tendbk
tsline lconsum lconsum_tendbk

*hp vs bk
tsline lpibhp lpibbk
tsline lipchp lipcbk
tsline ltinthp ltintbk
tsline lgastohp lgastobk
tsline latcamhp latcambk
tsline lconsumhp lconsumbk
*correlación ciclos hp
corr lpibhp lipchp ltinthp lgastohp latcamhp lconsumhp
xcorr lpibhp lipchp, tab
xcorr lpibhp lipchp,
xcorr lpibhp ltinthp, tab
xcorr lpibhp ltinthp
xcorr lpibhp lgastohp, tab
xcorr lpibhp lgastohp
xcorr lpibhp latcamhp, tab
xcorr lpibhp latcamhp
xcorr lpibhp lconsumhp, tab
xcorr lpibhp lconsumhp
*correlación ciclos bk
corr lpibbk lipcbk ltintbk lgastobk latcambk lconsumbk
xcorr lpibbk lipcbk, tab
xcorr lpibbk lipcbk
xcorr lpibbk ltintvk, tab
xcorr lpibbk ltintbk
xcorr lpibbk lgastobk, tab
xcorr lpibbk lgastobk
xcorr lpibbk latcambk, tab
xcorr lpibbk latcambk
xcorr lpibbk lconsumbk, tab
xcorr lpibbk lconsumbk
*IAE CICLO VS LOS DEMÁS CON HP 
tsline lpibhp lipchp
tsline lpibhp ltinthp
tsline lpibhp lgastohp
tsline lpibhp latcamhp
tsline lpibhp lconsumhp

*IAE CICLO VS LOS DEMÁS CON BK
 tsline lpibbk lipcbk
tsline lpibbk  ltintbk
tsline lpibbk  lgastobk
tsline lpibbk  latcambk
tsline lpibbk  lconsumbk
