library(data.table)
library(ensembleBMA)
library(Metrics)
library(ensembleMOS)
library(EBMAforecast)
library(keras)
library(neuralnet)
library(AMORE)
library(repmis)
library(ggplot2)
library(DEoptim)
library(lubridate)
library(keras)

setwd("~/Plocha/DATA_EXTRAKCE")
srfp=data.table(readRDS("data_kalibrace.rds"))  #ALADIN
srfp=data.table(readRDS("COSMO_ALADIN_kalibrace.rds")) #ALADIN_COSMO

srfp=srfp[format(ORIGIN, '%H:%M:%S')=='00:00:00',]
srfp=srfp[AHEAD==6]
srfp=srfp[ID==1060]
srfp=srfp[,c(1:42,44,45,43)]
srfp=srfp[,c(21:24):=NULL]
#srfp=srfp[1200:1400,]
#srfp=na.omit(srfp)

#row_sub = apply(srfp, 1, function(row) all(row !=0 ))
#srfp=srfp[row_sub,]

#### BMA #######################################################################################################
#memberLabels<-names(srfp[,5:21])  
memberLabels<-names(srfp[,5:41])   
cas <- as.POSIXct(srfp$ORIGIN, format = '%Y-%m-%d %H:%M:%S')
srfp$date <- format(cas, format = '%Y%m%d%H')

srfpData<-ensembleData(forecasts=srfp[,5:40], dates=srfp$date, observations=srfp$RAINFALL,initializationTime = "00", forecastHour = 6)
sum(is.na(srfpData))
srfpData=na.omit(srfpData)
#srfpData[,c(1:16,18)]=apply(srfpData[,c(1:16,18)],2,as.numeric)

#srfpData=srfpData[500:610,]
srfpFit<- ensembleBMA(srfpData,trainingDays = 50, model="gamma0",control = controlBMAgamma0()) 
        #srfpFit<- ensembleBMAgamma0(srfpData[500,],trainingDays = 100) 

#forecast  
#srfpFit<-fitBMAgamma0(srfpData[1:100,])
#srfpData[500,]

qf=quantileForecast.ensembleBMAgamma0(srfpFit, srfpData, quantiles = c( 0.1,0.3,0.5,0.7, 0.9))

# prirazeni dates ke qf
dates=srfp[,1:4]
dates=dates[51:length(dates$ORIGIN),] #odecteni training days - 50
dates=cbind(dates,qf)
saveRDS(dates,"qfCOSMO_06_1060")



    ##### AMORE ###########################################################

    setwd("~/Plocha/DATA_EXTRAKCE")
 #---Data ALADIN ###################################################################################################
    kaldat=readRDS("data_kalibrace.rds")
    kaldat=kaldat[AHEAD==06]
    kaldat=kaldat[ID==1060]
    kaldat=kaldat[,5:24]
    kaldat=kaldat[,c(1:18, 20, 19)]
    trainset <- kaldat[1:2000, ]
    testset <- kaldat[2001:length(kaldat$RAINFALL), ]
    
    trainset=as.matrix(trainset)
    sum(is.na(trainset))  #pocet NA v souboru
    trainset=na.omit(trainset) #odstraneni radku s NA
    
    net.start <- newff(n.neurons = c(20, 5, 3, 1),      
                       learning.rate.global = 1e-5,        
                       momentum.global = 0.08,              
                       error.criterium = 'LMS',           
                       Stao = NA,
                       hidden.layer = 'sigmoid',   
                       output.layer = 'purelin',           
                       method = 'ADAPTgdwm') 
    
    NN.train <- train( net.start, trainset[,c(1:19)], trainset[,20], error.criterium = 'LMS', report = TRUE, show.step = 500, n.shows = 50)
    NN.sim <- as.vector(sim(NN.train$net, kaldat[,c(1:19)]))
    vertab=data.table(sim=NN.sim, obs=kaldat[,20])
    res=NN.sim-kaldat[,20]
    
    sum(is.na(vertab$sim)) 
    sum(is.na(vertab$obs.RAINFALL)) 
    vertab=na.omit(vertab)
    cat="ALADIN"
  #---
    
  #---Data ALADIN+COSMO ############################################################################################ 
    kaldat=readRDS("COSMO_ALADIN_kalibrace.rds")
    kaldat=kaldat[AHEAD==06]
    kaldat=kaldat[ID==1060]
    tab=kaldat[,1:4]
    kaldat=kaldat[,5:45]   
    kaldat[,c(17,18,19,20)]<-NULL  #odstrani cleny cosmo 17, 18, 19 a 20, kterej sou az v pozdejsich letech
    kaldat=kaldat[,c(1:33,36,37,34,35)] #uspordani sloupcu
    sum(is.na(kaldat)) 
    kaldat=na.omit(kaldat)
    trainset <- kaldat[1:2000, ]
    testset <- kaldat[2001:length(kaldat$RAINFALL), ]
    
    trainset=as.matrix(trainset)
    sum(is.na(trainset))  #pocet NA v souboru
    trainset=na.omit(trainset) #odstraneni radku s NA
    
    net.start <- newff(n.neurons = c(37, 5, 3, 1),      
                       learning.rate.global = 1e-5,        
                       momentum.global = 0.08,              
                       error.criterium = 'LMS',           
                       Stao = NA,
                       hidden.layer = 'sigmoid',   
                       output.layer = 'purelin',           
                       method = 'ADAPTgdwm') 
    
    NN.train <- train( net.start, trainset[,c(1:36)], trainset[,37], error.criterium = 'LMS', report = TRUE, show.step = 500, n.shows = 50)
    NN.sim <- as.vector(sim(NN.train$net, kaldat[,c(1:36)]))
    vertab=data.table(sim=NN.sim, obs=kaldat[,37])
    res=NN.sim-kaldat[,37]
    cat="ALADIN_COSMO"
    
    sum(is.na(NN.sim)) 
    sum(is.na(vertab$obs.RAINFALL)) 
    
    #tab$LAEF_MEAN=apply(tab[,25:40], 1, mean)  #doplneni prumeru ensemblu
    #tab$COSMO_MEAN=apply(tab[,5:20], 1, mean)  #doplneni prumeru ensemblu
  #---   
    
  #--- Data ALADIN+COSMO t-1 t+1 ###########################################################################################
    kaldat=readRDS("COSMO_ALADIN_kalibrace.rds")
    ta=kaldat[ID==1060]
    ta[,43]<-NULL
    ta$AHEAD=(ta$AHEAD+6)
    ta$TIME=(ta$TIME)+21600
    ta=kaldat[AHEAD==06] 
   #ta$ORIGIN=(ta$ORIGIN)+21600
   # tb=kaldat[,c(3,4,43)]
   # tb=tb[!duplicated(tb),]
   # setkey(ta, ID, TIME)
   # setkey(tb, ID, TIME)
    #tplus1=ta[tb]
    tplus1=ta
    setnames(tplus1,old = c("COSMO_01","COSMO_02", "COSMO_03","COSMO_04","COSMO_05","COSMO_06","COSMO_07","COSMO_08","COSMO_09","COSMO_10","COSMO_11","COSMO_12","COSMO_13","COSMO_14","COSMO_15","COSMO_16","COSMO_17","COSMO_18","COSMO_19","COSMO_20"), new = c("COSMO_01+1","COSMO_02+1","COSMO_03+1","COSMO_04+1","COSMO_05+1","COSMO_06+1","COSMO_07+1","COSMO_08+1","COSMO_09+1","COSMO_10+1","COSMO_11+1","COSMO_12+1","COSMO_13+1","COSMO_14+1","COSMO_15+1","COSMO_16+1","COSMO_17+1","COSMO_18+1","COSMO_19+1","COSMO_20+1"))
    setnames(tplus1,old = c( "LAEF_1", "LAEF_2","LAEF_3","LAEF_4","LAEF_5","LAEF_6","LAEF_7","LAEF_8","LAEF_9","LAEF_10","LAEF_11","LAEF_12","LAEF_13","LAEF_14","LAEF_15","LAEF_16","LAEF_CF","ALADIN-CZ","COSMO_MEAN","LAEF_MEAN"), new = c( "LAEF_1+1", "LAEF_2+1","LAEF_3+1","LAEF_4+1","LAEF_5+1","LAEF_6+1","LAEF_7+1","LAEF_8+1","LAEF_9+1","LAEF_10+1","LAEF_11+1","LAEF_12+1","LAEF_13+1","LAEF_14+1","LAEF_15+1","LAEF_16+1","LAEF_CF+1","ALADIN-CZ+1","COSMO_MEAN+1","LAEF_MEAN+1"))    
   
  #---  
    kaldat=readRDS("COSMO_ALADIN_kalibrace.rds")
    ta=kaldat[ID==1060]
    ta[,43]<-NULL
    ta$AHEAD=(ta$AHEAD-6)
    ta$TIME=(ta$TIME)-21600 
    ta=ta[AHEAD==48]
    #ta$AHEAD=ta$AHEAD-6
    #ta$ORIGIN=(ta$ORIGIN)-21600
    #tb=kaldat[,c(3,4,43)]
    #tb=tb[!duplicated(tb),]
   # setkey(ta, ID, TIME)
   # setkey(tb, ID, TIME)
    #tminus1=ta[tb]
    tminus1=ta
    setnames(tminus1,old = c("COSMO_01","COSMO_02", "COSMO_03","COSMO_04","COSMO_05","COSMO_06","COSMO_07","COSMO_08","COSMO_09","COSMO_10","COSMO_11","COSMO_12","COSMO_13","COSMO_14","COSMO_15","COSMO_16","COSMO_17","COSMO_18","COSMO_19","COSMO_20"), new = c("COSMO_01-1","COSMO_02-1","COSMO_03-1","COSMO_04-1","COSMO_05-1","COSMO_06-1","COSMO_07-1","COSMO_08-1","COSMO_09-1","COSMO_10-1","COSMO_11-1","COSMO_12-1","COSMO_13-1","COSMO_14-1","COSMO_15-1","COSMO_16-1","COSMO_17-1","COSMO_18-1","COSMO_19-1","COSMO_20-1"))
    setnames(tminus1,old = c( "LAEF_1", "LAEF_2","LAEF_3","LAEF_4","LAEF_5","LAEF_6","LAEF_7","LAEF_8","LAEF_9","LAEF_10","LAEF_11","LAEF_12","LAEF_13","LAEF_14","LAEF_15","LAEF_16","LAEF_CF","ALADIN-CZ","COSMO_MEAN","LAEF_MEAN"), new = c( "LAEF_1-1", "LAEF_2-1","LAEF_3-1","LAEF_4-1","LAEF_5-1","LAEF_6-1","LAEF_7-1","LAEF_8-1","LAEF_9-1","LAEF_10-1","LAEF_11-1","LAEF_12-1","LAEF_13-1","LAEF_14-1","LAEF_15-1","LAEF_16-1","LAEF_CF-1","ALADIN-CZ-1","COSMO_MEAN-1","LAEF_MEAN-1"))    
    
   #---
    
  setkey(tminus1, ID, AHEAD, TIME, ORIGIN)
  setkey(tplus1, ID, AHEAD, TIME, ORIGIN)
  setkey(kaldat, ID, AHEAD, TIME, ORIGIN)
    
  tplusminus=kaldat[tminus1]
  tplusminus=tplusminus[tplus1]
  tplusminus[,c(21:24, 62:65)]<-NULL
  
  tplusminus=tplusminus[!is.na(tplusminus$ORIGIN), ]   #odstrani radky s NA ale zachova NA u COSMO 17-20

  kaldat=tplusminus[,c(5:38,40:77,39)] #uspordani sloupcu
  sum(is.na(kaldat)) 
  kaldat=na.omit(kaldat)
  trainset <- kaldat[1:2000, ]
  testset <- kaldat[2001:length(kaldat$RAINFALL), ]
  
  trainset=as.matrix(trainset)
  sum(is.na(trainset))  #pocet NA v souboru
  trainset=na.omit(trainset) #odstraneni radku s NA
  
  net.start <- newff(n.neurons = c(72, 5, 3, 1),      
                     learning.rate.global = 1e-5,        
                     momentum.global = 0.08,              
                     error.criterium = 'LMS',           
                     Stao = NA,
                     hidden.layer = 'sigmoid',   
                     output.layer = 'purelin',           
                     method = 'ADAPTgdwm') 
  
  NN.train <- train( net.start, trainset[,c(1:72)], trainset[,73], error.criterium = 'LMS', report = TRUE, show.step = 500, n.shows = 50)
  NN.sim <- as.vector(sim(NN.train$net, kaldat[,c(1:72)]))
  vertab=data.table(sim=NN.sim, obs=kaldat[,73])
  res=NN.sim-kaldat[,73]
  cat="ALADIN_COSMO_T-1_AHEAD48"
  #r=tplusminus[,c("ORIGIN","AHEAD", "LAEF_2","LAEF_2-1","LAEF_2+1",  "RAINFALL","RAINFALL-1","RAINFALL+1")]
  
     
 # Kvantil ALADIN- Pridani kvantilu pro 06 AHEAD a 1060 ID ###########################################################################################
  
  kaldat=readRDS("data_kalibrace.rds")
  kaldat=kaldat[AHEAD==06]
  kaldat=kaldat[ID==1060]
  setwd("~/Plocha/DATA_EXTRAKCE")
  qf=readRDS("qf_06_1060")
  setkey(kaldat,ORIGIN, AHEAD, ID, TIME)
  setkey(qf,ORIGIN, AHEAD, ID, TIME)
  kaldatqf=kaldat[qf]
  sum(is.na(kaldatqf)) 
  kaldatqf=na.omit(kaldatqf)
  kaldatqf=kaldatqf[,c(1:22,24:29,23)] #uspordani sloupcu
  kaldatqf=kaldatqf[,5:29]    
  trainset <- kaldatqf[1:2000, ]
  testset <- kaldatqf[2001:length(kaldat$RAINFALL), ]
  
      
    trainset=as.matrix(trainset)
    sum(is.na(trainset))  #pocet NA v souboru
    trainset=na.omit(trainset) #odstraneni radku s NA
    
    net.start <- newff(n.neurons = c(25, 5, 3, 1),      
                       learning.rate.global = 1e-5,        
                       momentum.global = 0.08,              
                       error.criterium = 'LMS',           
                       Stao = NA,
                       hidden.layer = 'sigmoid',   
                       output.layer = 'purelin',           
                       method = 'ADAPTgdwm') 
   
    NN.train <- train( net.start, trainset[,c(1:24)], trainset[,25], error.criterium = 'LMS', report = TRUE, show.step = 500, n.shows = 50)
    NN.sim <- as.vector(sim(NN.train$net, kaldatqf[,c(1:24)]))
    vertab=data.table(sim=NN.sim, obs=kaldatqf[,25])
    res=NN.sim-kaldatqf[,25]
    cat="ALADIN_QF"
    
    sum(is.na(NN.sim)) 
    sum(is.na(vertab$obs.RAINFALL)) 
    
    
### Kvantil ALADIN+COSMO #######
    kaldat=readRDS("COSMO_ALADIN_kalibrace.rds")
    kaldat=kaldat[AHEAD==06]
    kaldat=kaldat[ID==1060]
    kaldat[,c(21:24)]<-NULL  #odstrani cleny cosmo 17, 18, 19 a 20, kterej sou az v pozdejsich letech
    kaldat=kaldat[,c(1:38,40,41,39)] #uspordani sloupcu
    sum(is.na(kaldat)) 
    kaldat=na.omit(kaldat)
    setwd("~/Plocha/DATA_EXTRAKCE")
    qf=readRDS("qfCOSMO_06_1060")
    setkey(kaldat,ORIGIN, AHEAD, ID, TIME)
    setkey(qf,ORIGIN, AHEAD, ID, TIME)
    kaldatqf=kaldat[qf]
    sum(is.na(kaldatqf)) 
    kaldatqf=na.omit(kaldatqf)
     tab=kaldatqf[,1:4] #pro verifikacni tabulku
    kaldatqf=kaldatqf[,c(1:40,42:46,41)] #uspordani sloupcu
    kaldatqf=kaldatqf[,5:46]    
   
    trainset <- kaldatqf[1:1000, ]
    testset <- kaldatqf[1001:length(kaldat$RAINFALL), ]
    
    trainset=as.matrix(trainset)
    sum(is.na(trainset))  #pocet NA v souboru
    trainset=na.omit(trainset) #odstraneni radku s NA
    
    net.start <- newff(n.neurons = c(41, 5, 3, 1),      
                       learning.rate.global = 1e-5,        
                       momentum.global = 0.08,              
                       error.criterium = 'LMS',           
                       Stao = NA,
                       hidden.layer = 'sigmoid',   
                       output.layer = 'purelin',           
                       method = 'ADAPTgdwm') 
    
    NN.train <- train( net.start, trainset[,c(1:41)], trainset[,42], error.criterium = 'LMS', report = TRUE, show.step = 500, n.shows = 50)
    NN.sim <- as.vector(sim(NN.train$net, kaldatqf[,c(1:41)]))
    vertab=data.table(sim=NN.sim, obs=kaldatqf[,42])
    res=NN.sim-kaldatqf[,42]
    cat="ALADIN_COSMO_QF"
    
    sum(is.na(NN.sim)) 
    sum(is.na(vertab$obs.RAINFALL)) 
    
##### Data ALADIN + COSMO + Q ######
    kaldat=readRDS("COSMO_ALADIN_Q_kalibrace")
    kaldat=kaldat[AHEAD==06]
    kaldat=kaldat[ID==1060]
    tab=kaldat[,1:4]
    kaldat=kaldat[,5:46]   
    kaldat[,c(17,18,19,20)]<-NULL  #odstrani cleny cosmo 17, 18, 19 a 20, kterej sou az v pozdejsich letech
    kaldat=kaldat[,c(1:33,36,37,34,38,35)] #uspordani sloupcu
    sum(is.na(kaldat)) 
    kaldat=na.omit(kaldat)
    trainset <- kaldat[1:2000, ]
    testset <- kaldat[2001:length(kaldat$RAINFALL), ]
    
    trainset=as.matrix(trainset)
    sum(is.na(trainset))  #pocet NA v souboru
    trainset=na.omit(trainset) #odstraneni radku s NA
    
    net.start <- newff(n.neurons = c(38, 5, 3, 1),      
                       learning.rate.global = 1e-5,        
                       momentum.global = 0.08,              
                       error.criterium = 'LMS',           
                       Stao = NA,
                       hidden.layer = 'sigmoid',   
                       output.layer = 'purelin',           
                       method = 'ADAPTgdwm') 
    
    NN.train <- train( net.start, trainset[,c(1:37)], trainset[,38], error.criterium = 'LMS', report = TRUE, show.step = 500, n.shows = 50)
    NN.sim <- as.vector(sim(NN.train$net, kaldat[,c(1:37)]))
    vertab=data.table(sim=NN.sim, obs=kaldat[,38])
    res=NN.sim-kaldat[,38]
    cat="ALADIN_COSMO_Q"
    
    sum(is.na(NN.sim)) 
    sum(is.na(vertab$obs.RAINFALL)) 

#### Kvantil (ALADIN + COSMO) + Q ######
    
    kaldat=readRDS("COSMO_ALADIN_Q_kalibrace")
    kaldat=kaldat[AHEAD==06]
    kaldat=kaldat[ID==1060]
    kaldat[,c(21:24)]<-NULL  #odstrani cleny cosmo 17, 18, 19 a 20, kterej sou az v pozdejsich letech
    kaldat=kaldat[,c(1:38,40,41,42,39)] #uspordani sloupcu
    sum(is.na(kaldat)) 
    kaldat=na.omit(kaldat)
    setwd("~/Plocha/DATA_EXTRAKCE")
    qf=readRDS("qfCOSMO_06_1060")
    qf$TIME=as.character(qf$TIME)
    setkey(kaldat,ORIGIN, AHEAD, ID, TIME)
    setkey(qf,ORIGIN, AHEAD, ID, TIME)
    kaldatqf=kaldat[qf]
    sum(is.na(kaldatqf)) 
    kaldatqf=na.omit(kaldatqf)
    tab=kaldatqf[,1:4] #pro verifikacni tabulku
    kaldatqf=kaldatqf[,c(1:41,43:47,42)] #uspordani sloupcu
    kaldatqf=kaldatqf[,5:47]    
    
    trainset <- kaldatqf[1:1000, ]
    testset <- kaldatqf[1001:length(kaldat$RAINFALL), ]
    
    trainset=as.matrix(trainset)
    sum(is.na(trainset))  #pocet NA v souboru
    trainset=na.omit(trainset) #odstraneni radku s NA
    
    net.start <- newff(n.neurons = c(42, 5, 3, 1),      
                       learning.rate.global = 1e-5,        
                       momentum.global = 0.08,              
                       error.criterium = 'LMS',           
                       Stao = NA,
                       hidden.layer = 'sigmoid',   
                       output.layer = 'purelin',           
                       method = 'ADAPTgdwm') 
    
    NN.train <- train( net.start, trainset[,c(1:42)], trainset[,43], error.criterium = 'LMS', report = TRUE, show.step = 500, n.shows = 50)
    NN.sim <- as.vector(sim(NN.train$net, kaldatqf[,c(1:42)]))
    vertab=data.table(sim=NN.sim, obs=kaldatqf[,43])
    res=NN.sim-kaldatqf[,43]
    cat="ALADIN_COSMO_QF_Q"  
    
##### Verifikace ####################################################################################################################################  
   
     vertab=data.table(sim=NN.sim, obs=kaldat[,37])
    res=NN.sim-kaldat[,37]
    
    sum(is.na(NN.sim)) 
    sum(is.na(vertab$obs.RAINFALL)) 
   
    
  RMSE= rmse(as.vector(vertab$obs.RAINFALL), as.vector(vertab$sim))
    vertab$RMSE=RMSE
  MAE=mae(as.vector(vertab$obs.RAINFALL), as.vector(vertab$sim))
  ME=mean(vertab$obs.RAINFALL-vertab$sim)
  data.table(RMSE,MAE, ME) 
  tab=data.table(cat,RMSE,MAE, ME) 
  tab
    
 # eval=data.table()
  eval=rbind(eval,tab)
  
    saveRDS(eval, "evaltab.rds") 
    eval=readRDS("evaltab.rds")
  
    
      plot(NN.sim, type="l", ylim=c(-1,max(kaldat[,37])), col="red")
    lines( kaldat[,37],col="blue")
    lines(res,type="l", col="orange")

    
        
  # Porovnani simulaci
    
    simtab=readRDS("simtab.rds")
   # tab=kaldat[,1:4]    #hned po nacteni kaldat a selekci ahead a ID, pred odstranenim identifikatoru
    tab[,ALADIN_COSMO_qf:=NN.sim]
    setkey(tab,ORIGIN,AHEAD,TIME,ID )
    setkey(simtab,ORIGIN,AHEAD,TIME,ID )
   
    simtab=simtab[tab]
    saveRDS(simtab, "simtab")
    
      ###### KERAS #########
    
    ### EMOS #####
    
    srfpFitE<- ensembleMOScsg0(srfpData,trainingDays =30) 
    srfpFitE<- fitMOS(srfpData, model = 'csg0')
    
    
    
    
    ### NEURALNET ##############################################################
    kaldat=readRDS("data_kalibrace.rds")
    
    kaldat=kaldat[AHEAD==6]
    kaldat=kaldat[ID==1060]
    kaldat=kaldat[,5:23]
    
    ############### v pripade ruznych jednotek je potreba Normalizace
    normalize <- function(x) {
      return ((x - min(x)) / (max(x) - min(x)))
    }
    
    maxmindf <- as.data.frame(lapply(kaldat, normalize))
    trainset <- maxmindf[1:2000, ]
    testset <- maxmindf[2001:length(kaldat$TIME), ]
    ###################################################################
    trainset <- kaldat[1:2000, ]
    testset <- kaldat[2001:length(kaldat$RAINFALL), ]
    
    nn <- neuralnet(RAINFALL~ LAEF_1 + LAEF_2+ LAEF_3 + LAEF_4 + LAEF_5 + LAEF_6 + LAEF_7 + LAEF_8 + LAEF_9 + LAEF_10 +
                      LAEF_11 + LAEF_12 + LAEF_13 + LAEF_14 + LAEF_15 + LAEF_16, data=trainset, hidden=c(1), linear.output=FALSE, threshold=0.01)
    nn$result.matrix
    plot(nn)
    
    #Test the resulting output
    temp_test <- subset(testset, select = c("LAEF_1", "LAEF_2","LAEF_3", "LAEF_4", "LAEF_5", "LAEF_6", "LAEF_7", "LAEF_8", "LAEF_9", "LAEF_10", "LAEF_11", "LAEF_12", "LAEF_13", "LAEF_14", "LAEF_15", "LAEF_16"))
    head(temp_test)
    nn.results <- compute(nn, temp_test)
    nn.results = predict(nn, temp_test)
    results <- data.frame(actual = testset$RAINFALL, prediction = nn.results$net.result)
    results  
    
 #### Nacteni prutoku ####
nam=list.files(pattern=".txt")
Qtab=data.table()    
for (i in seq_along(nam))  {
q<-data.table(read.table(file = nam[i], header = FALSE, sep = ",", fileEncoding = "UTF-8", na.strings = "", stringsAsFactors = FALSE, col.names = c("ID","a","b","TIME","Q", "c")))
q[,c("a","b","c")]<-NULL
id=as.numeric(substr(nam[i], start = 1, stop = 4))
q[,ID:=id]
cas <- as.POSIXct(q$TIME, format = "%d.%m.%Y %H")
q$TIME <- format(cas, format ='%Y-%m-%d %H:%M:%S' )
Qtab=rbind(Qtab,q)
}    

Qtab$ID=as.factor(Qtab$ID)
#Qtab$TIME=as.Date(Qtab$TIME)
kaldat$TIME=as.character(kaldat$TIME)
saveRDS(Qtab,"prutoky_1h")  


setkey(kaldat, ID, TIME)
setkey(Qtab, ID, TIME)
kaldatQ=Qtab[kaldat]
kaldatQ=kaldatQ[,c(1:2,4:46,3)]
saveRDS(kaldatQ, "COSMO_ALADIN_Q_kalibrace")
