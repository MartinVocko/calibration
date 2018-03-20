setwd("~/Plocha/DATA/tab_RDS")
srfp=readRDS("vertab.RDS")

#aux <- as.POSIXlt('1988-1-2 00:00:00', format = '%Y-%m-%d %H:%M:%S')
#format(aux, '%H:%M:%S')

srfp=srfp[format(ORIGIN, '%H:%M:%S')=='00:00:00',]
srfp=srfp[AHEAD==6]
#srfp=na.omit(srfp)

#row_sub = apply(srfp, 1, function(row) all(row !=0 ))
#srfp=srfp[row_sub,]

#### BMA ######
memberLabels<-names(srfp[,11:26])    
cas <- as.POSIXct(srfp$ORIGIN, format = '%Y-%m-%d %H:%M:%S')
srfp$date <- format(cas, format = '%Y%m%d%H')

srfpData<-ensembleData(forecasts=srfp[,11:26], dates=srfp$date, observations=srfp$PR, 
                       latitude=srfp$ID, longitude=srfp$ID,initializationTime = "00", forecastHour = 6)


srfpData[,c(1:16,18)]=apply(srfpData[,c(1:16,18)],2,as.numeric)


srfpFit<- ensembleBMA(srfpData,trainingDays = 30, model="gamma0",control = controlBMAgamma0()) #
srfpFit<- ensembleBMAgamma0(srfpData,trainingDays = 30, control = controlBMAgamma0( )) 

srfpFit<-fitBMAgamma0(srfpData)



### EMOS #####

srfpFitE<- ensembleMOScsg0(srfpData,trainingDays =30) 
srfpFit<- fitMOS(srfpData, model = 'csg0')

