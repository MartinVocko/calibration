setwd("~/Plocha/DATA/tab_RDS")
srfp=readRDS("vertab.RDS")

#### BMA ######
memberLabels<-names(srfp[,11:26])    
cas <- as.POSIXct(srfp$TIME, format = '%Y-%m-%d %H:%M:%S')
srfp$date <- format(cas, format = '%Y%m%d%H')

srfpData<-ensembleData(forecasts=srfp[,11:26],dates=srfp$date, observations=srfp$PR, 
                       latitude=srfp$ID, longitude=srfp$ID, initializationTime = c("00","12"), forecastHour = 54)


srfpFit<- ensembleBMA(srfpData,trainingDays = 30, model="gamma0",control = controlBMAgamma()) #
srfpFit<- ensembleBMAgamma0(srfpData,trainingDays = 29) 



### EMOS #####S

srfpFit<- ensembleMOScsg0(srfpData,trainingDays = 30) 
